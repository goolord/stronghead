-- | The ghc-debug side of "Stronghead.Debug": connect to a ghc-debug-stub,
-- pause the program, walk the closure graph breadth-first from the GC roots,
-- and resume. Only built with the @ghc-debug@ flag.
module Stronghead.Debug.Client
  ( walkTarget
  , listSockets
  ) where

import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Tracer (nullTracer)
import Data.Bitraversable (bitraverse)
import Data.List (sort)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Debug.Client
import GHC.Debug.Client.Monad (debuggeeConnectWithTracer, debuggeeConnectWithTracerTCP, snapshotInitWithTracer)
import GHC.Debug.Types.Ptr (ClosurePtr (..), mkClosurePtr)
import Stronghead.Debug.Graph
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | The roots, the closures reached from them, and whether the traversal
-- stopped at its limit. A live target is paused for the walk and resumed
-- when it ends, even if the walk fails.
walkTarget :: SnapshotOptions -> DebugTarget -> IO ([ClosureId], [ClosureNode], Bool)
walkTarget opts = \case
  DebugUnix path -> live (debuggeeConnectWithTracer nullTracer path)
  DebugTCP host port -> live (debuggeeConnectWithTracerTCP nullTracer host port)
  DebugSnapshotFile path -> do
    e <- snapshotInitWithTracer nullTracer path
    run e (walk opts)
  where
    -- Closing the connection resumes the program.
    live connect = bracket connect debuggeeClose $ \e -> do
      pause e
      run e (walk opts)

walk :: SnapshotOptions -> DebugM ([ClosureId], [ClosureNode], Bool)
walk SnapshotOptions {..} = do
  _ <- precacheBlocks
  roots <- gcRoots
  let rootIds = map ptrId roots
      go Empty _ _ acc = pure (acc, False)
      go queue@(p :<| rest) seen n acc
        | n >= soMaxClosures = pure (acc, not (Seq.null queue))
        | otherwise = do
            node <- decode soFollowSrt p
            let new = S.toList (S.fromList [c | c <- cnChildren node, not (S.member c seen)])
                seen' = foldr S.insert seen new
            go (rest <> Seq.fromList (map mkClosurePtr new)) seen' (n + 1) (node : acc)
  (nodes, truncated) <- go (Seq.fromList roots) (S.fromList rootIds) (0 :: Int) []
  pure (rootIds, reverse nodes, truncated)

ptrId :: ClosurePtr -> ClosureId
ptrId (ClosurePtr w) = w

decode :: Bool -> ClosurePtr -> DebugM ClosureNode
decode followSrt p = do
  sc <- dereferenceClosure p
  let srt = if followSrt then dereferenceSRT else const (pure (GenSrtPayload Nothing))
  c <- hextraverse pure srt dereferencePapPayload dereferenceConDesc (bitraverse srt pure <=< dereferenceStack) pure sc
  let cl = noSize c
      itb = info cl
      ty = T.pack (show (tipe (decodedTable itb)))
  src <- getSourceInfo (tableId itb)
  let name = case cl of
        ConstrClosure {constrDesc = ConstrDesc {name = n}} -> T.pack n
        TSOClosure {threadId = t} -> "thread " <> T.pack (show t)
        _ -> maybe ty sourceName src
      kids = case cl of
        -- A weak pointer does not keep its key or value alive.
        WeakClosure {} -> []
        _ -> map ptrId (allClosures cl)
  pure
    ClosureNode
      { cnId = ptrId p
      , cnType = ty
      , cnKind = kindOfClosureType ty
      , cnName = name
      , cnSource = sourceSpan =<< src
      , cnSize = getSize (dcSize c)
      , cnChildren = length kids `seq` kids
      }

sourceName :: SourceInformation -> Text
sourceName si
  | not (null (infoLabel si)) = T.pack (infoLabel si)
  | otherwise = T.pack (infoName si)

sourceSpan :: SourceInformation -> Maybe Text
sourceSpan si
  | null (infoPosition si) = Nothing
  | otherwise = Just (T.pack (infoPosition si))

-- | Sockets of programs started with 'GHC.Debug.Stub.withGhcDebug', which
-- creates them in ghc-debug's socket directory.
listSockets :: IO [FilePath]
listSockets = do
  dir <- socketDirectory
  ok <- doesDirectoryExist dir
  if ok then map (dir </>) . sort <$> listDirectory dir else pure []
