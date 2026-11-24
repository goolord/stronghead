-- | Cross-platform in-process heap closure & thunk probing via @ghc-heap@ and
-- @GHC.InfoProv.whereFrom@.
--
-- Works on Windows, macOS, and Linux without @ghc-debug-stub@ or pausing the
-- entire RTS: register named @IORef@s, @TVar@s, @MVar@s, or values with
-- "Stronghead.Agent", and inspect their live WHNF vs thunk structure, source
-- provenance (when built with @-finfo-table-map@), and thunk chain depth on
-- demand.
module Stronghead.Probe
  ( ProbeOptions (..)
  , defaultProbeOptions
  , inspectBox
  , inspectValue
  , probeRef
  , probeTVar
  , probeMVar
  , probeValue
    -- * Weak-pointer non-retaining mutable cell probes
  , probeWeakRef
  , probeWeakTVar
  , probeWeakMVar
  , newProbedIORef
  , newProbedTVarIO
  , newProbedMVar
  ) where

import Control.Concurrent.MVar (MVar, mkWeakMVar, newMVar, tryReadMVar)
import Control.Concurrent.STM (TVar, mkWeakTVar, newTVarIO, readTVarIO)
import Data.Bits (xor)
import Data.Char (ord)
import Data.IORef (IORef, mkWeakIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import GHC.Exts.Heap
  ( Box (..)
  , Closure
  , GenClosure (..)
  , areBoxesEqual
  , asBox
  , getBoxedClosureData
  )
import GHC.Exts.Heap.InfoTable.Types (StgInfoTable (..))
import GHC.InfoProv (InfoProv (..), ipLoc, whereFrom)
import Numeric (showHex)
import System.Mem.Weak (Weak, deRefWeak)

import Stronghead.Agent (ProbeNode (..), registerProbe, unregisterProbe)

data ProbeOptions = ProbeOptions
  { poMaxNodes :: !Int
    -- ^ Maximum closures to walk per probe snapshot.
  , poMaxDepth :: !Int
    -- ^ Maximum pointer-following depth from the probed root.
  }
  deriving stock (Eq, Show)

defaultProbeOptions :: ProbeOptions
defaultProbeOptions = ProbeOptions {poMaxNodes = 128, poMaxDepth = 16}

-- | Inspect any Haskell value lazily (without forcing it!) into a bounded
-- list of 'ProbeNode's.
inspectValue :: ProbeOptions -> a -> IO [ProbeNode]
inspectValue opts x = inspectBox opts (asBox x)

-- | Inspect a boxed closure lazily via @ghc-heap@ and @GHC.InfoProv@.
-- Preserves DAG sharing and breaks cycles via pointer identity ('areBoxesEqual')
-- without forcing any thunks, emitting @SHARED_REF@ link nodes for shared
-- pointers and 64-bit FNV-1a payload digests for primitive / @ARR_WORDS@ data.
inspectBox :: ProbeOptions -> Box -> IO [ProbeNode]
inspectBox ProbeOptions {..} root = go [(0, -1, 0, root)] 1 [] []
  where
    findSeen _ [] = pure Nothing
    findSeen b ((sid, sb) : rest) = do
      same <- areBoxesEqual b sb
      if same then pure (Just sid) else findSeen b rest

    go [] _ _ acc = pure (reverse acc)
    go ((nid, parent, depth, box@(Box raw)) : queue) nextId seen acc
      | length acc >= poMaxNodes = pure (reverse acc)
      | otherwise = do
          mExisting <- findSeen box seen
          case mExisting of
            Just targetId -> do
              let linkNode =
                    ProbeNode
                      { pnId = nid
                      , pnParent = parent
                      , pnType = "SHARED_REF"
                      , pnName = "-> #" <> show targetId
                      , pnSource = ""
                      , pnBytes = 0
                      , pnIsThunk = False
                      , pnDetail = "shared=" <> show targetId
                      }
              go queue nextId seen (linkNode : acc)
            Nothing -> do
              cl <- getBoxedClosureData box
              mProv <- whereFrom raw
              let (ty, name, bytes, isThunk, detail0, mPayHash, kids) = describeClosure cl mProv
                  detail = case mPayHash of
                    Nothing -> detail0
                    Just h ->
                      let hTag = "hash=0x" <> showHex h ""
                       in if null detail0 then hTag else detail0 <> " " <> hTag
                  src = maybe "" renderProvLoc mProv
                  node =
                    ProbeNode
                      { pnId = nid
                      , pnParent = parent
                      , pnType = ty
                      , pnName = name
                      , pnSource = src
                      , pnBytes = bytes
                      , pnIsThunk = isThunk
                      , pnDetail = detail
                      }
                  room = max 0 (poMaxNodes - (length acc + 1 + length queue))
                  childList =
                    if depth >= poMaxDepth
                      then []
                      else take room kids
                  childItems =
                    [ (cid, nid, depth + 1, k)
                    | (cid, k) <- zip [nextId ..] childList
                    ]
                  nextId' = nextId + length childItems
              go (queue ++ childItems) nextId' ((nid, box) : seen) (node : acc)

-- | Register an 'IORef' whose current contents will be probed lazily on
-- demand (the contents themselves are NOT forced when probing).
probeRef :: String -> IORef a -> IO ()
probeRef name ref =
  registerProbe name (readIORef ref >>= inspectValue defaultProbeOptions)

-- | Register a 'TVar' whose current contents will be probed lazily on demand.
probeTVar :: String -> TVar a -> IO ()
probeTVar name tv =
  registerProbe name (readTVarIO tv >>= inspectValue defaultProbeOptions)

-- | Register an 'MVar' whose current contents (when full) will be probed
-- lazily on demand without blocking.
probeMVar :: String -> MVar a -> IO ()
probeMVar name mv =
  registerProbe name $ do
    mx <- tryReadMVar mv
    case mx of
      Nothing -> pure [ProbeNode 0 (-1) "MVAR_EMPTY" name "" 32 False "empty MVar"]
      Just x -> inspectValue defaultProbeOptions x

-- | Register any value lazily as a live probe.
probeValue :: String -> a -> IO ()
probeValue name x =
  let !b = asBox x
   in registerProbe name (inspectBox defaultProbeOptions b)

-- ---------------------------------------------------------------------------
-- Weak-pointer non-retaining mutable cell probes

registerWeakProbe :: String -> Weak cell -> (cell -> IO [ProbeNode]) -> IO ()
registerWeakProbe name wk inspectCell =
  registerProbe name $ do
    mc <- deRefWeak wk
    case mc of
      Nothing -> unregisterProbe name >> pure []
      Just cell -> inspectCell cell

-- | Register an 'IORef' via a weak pointer so Stronghead never prevents the
-- 'IORef' from being garbage-collected; automatically unregisters on GC.
probeWeakRef :: String -> IORef a -> IO ()
probeWeakRef name ref = do
  wk <- mkWeakIORef ref (unregisterProbe name)
  registerWeakProbe name wk (readIORef >=> inspectValue defaultProbeOptions)
  where
    (f >=> g) x = f x >>= g

-- | Register a 'TVar' via a weak pointer without retaining it across GC.
probeWeakTVar :: String -> TVar a -> IO ()
probeWeakTVar name tv = do
  wk <- mkWeakTVar tv (unregisterProbe name)
  registerWeakProbe name wk (\c -> readTVarIO c >>= inspectValue defaultProbeOptions)

-- | Register an 'MVar' via a weak pointer without retaining it across GC.
probeWeakMVar :: String -> MVar a -> IO ()
probeWeakMVar name mv = do
  wk <- mkWeakMVar mv (unregisterProbe name)
  registerWeakProbe name wk $ \c -> do
    mx <- tryReadMVar c
    case mx of
      Nothing -> pure [ProbeNode 0 (-1) "MVAR_EMPTY" name "" 32 False "empty MVar"]
      Just x -> inspectValue defaultProbeOptions x

-- | Drop-in replacement for 'newIORef' that automatically registers a non-retaining weak probe.
newProbedIORef :: String -> a -> IO (IORef a)
newProbedIORef name val = do
  r <- newIORef val
  probeWeakRef name r
  pure r

-- | Drop-in replacement for 'newTVarIO' that automatically registers a non-retaining weak probe.
newProbedTVarIO :: String -> a -> IO (TVar a)
newProbedTVarIO name val = do
  tv <- newTVarIO val
  probeWeakTVar name tv
  pure tv

-- | Drop-in replacement for 'newMVar' that automatically registers a non-retaining weak probe.
newProbedMVar :: String -> a -> IO (MVar a)
newProbedMVar name val = do
  mv <- newMVar val
  probeWeakMVar name mv
  pure mv

-- ---------------------------------------------------------------------------
-- Closure decoding

fnvWords :: [Word] -> Maybe Word64
fnvWords [] = Nothing
fnvWords ws = Just (foldl' (\h w -> (h `xor` fromIntegral w) * 1099511628211) 14695981039346656037 (take 64 ws))

fnvStr :: String -> Word64
fnvStr = foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) 14695981039346656037

describeClosure :: Closure -> Maybe InfoProv -> (String, String, Int, Bool, String, Maybe Word64, [Box])
describeClosure cl mProv = case cl of
  ConstrClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (1 + length ptrArgs + length dataArgs)
        det =
          (if null modl then "" else modl <> ".")
            <> name
            <> (if null dataArgs then "" else " data=" <> show (take 4 dataArgs))
     in (ty, if null name then provName ty else name, sz, False, det, fnvWords dataArgs, ptrArgs)
  ThunkClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (2 + length ptrArgs + length dataArgs)
        det = "ptrs=" <> show (length ptrArgs) <> " words=" <> show (length dataArgs)
     in (ty, provName ty, sz, True, det, fnvWords dataArgs, ptrArgs)
  SelectorClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "THUNK_SELECTOR", 16, True, "selector thunk", Nothing, [selectee])
  APClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (3 + length payload)
     in (ty, provName "AP", sz, True, "arity=" <> show arity <> " applied=" <> show n_args, Nothing, fun : payload)
  APStackClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (2 + length payload)
     in (ty, provName "AP_STACK", sz, True, "stack_ptrs=" <> show (length payload), Nothing, fun : payload)
  PAPClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (3 + length payload)
     in (ty, provName "PAP", sz, False, "arity=" <> show arity <> " applied=" <> show n_args, Nothing, fun : payload)
  FunClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (1 + length ptrArgs + length dataArgs)
     in (ty, provName ty, sz, False, "free_ptrs=" <> show (length ptrArgs), fnvWords dataArgs, ptrArgs)
  IndClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "IND", 16, False, "indirection", Nothing, [indirectee])
  BlackholeClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "BLACKHOLE", 16, True, "under evaluation", Nothing, [indirectee])
  ArrWordsClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "ARR_WORDS", 16 + fromIntegral bytes, False, show bytes <> " bytes", fnvWords arrWords, [])
  MutArrClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "MUT_ARR_PTRS", 16 + 8 * fromIntegral mccSize, False, show mccPtrs <> " ptrs", Nothing, take 32 mccPayload)
  SmallMutArrClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "SMALL_MUT_ARR_PTRS", 16 + 8 * fromIntegral mccPtrs, False, show mccPtrs <> " ptrs", Nothing, take 32 mccPayload)
  MVarClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "MVAR", 32, False, "MVar", Nothing, [value])
  MutVarClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "MUT_VAR", 16, False, "IORef/MutVar", Nothing, [var])
  IntClosure {..} ->
    ("INT", show intVal, 16, False, "Int", Just (fnvStr (show intVal)), [])
  WordClosure {..} ->
    ("WORD", show wordVal, 16, False, "Word", Just (fnvStr (show wordVal)), [])
  Int64Closure {..} ->
    ("INT64", show int64Val, 16, False, "Int64", Just (fnvStr (show int64Val)), [])
  Word64Closure {..} ->
    ("WORD64", show word64Val, 16, False, "Word64", Just (fnvStr (show word64Val)), [])
  AddrClosure {..} ->
    ("ADDR", show addrVal, 16, False, "Addr", Just (fnvStr (show addrVal)), [])
  DoubleClosure {..} ->
    ("DOUBLE", show doubleVal, 16, False, "Double", Just (fnvStr (show doubleVal)), [])
  FloatClosure {..} ->
    ("FLOAT", show floatVal, 16, False, "Float", Just (fnvStr (show floatVal)), [])
  UnknownTypeWordSizedPrimitive {..} ->
    ("PRIM_WORD", show wordVal, 8, False, "word-sized primitive", Just (fnvWords' wordVal), [])
  other ->
    let ty = closureTypeName other
     in (ty, provName ty, 16, False, "", Nothing, [])
  where
    fnvWords' w = (14695981039346656037 `xor` fromIntegral w) * 1099511628211
    provName fallback = fromMaybe fallback $ do
      ip <- mProv
      pure (if null (ipLabel ip) then ipName ip else ipLabel ip)

closureTypeName :: Closure -> String
closureTypeName = \case
  WeakClosure {} -> "WEAK"
  TSOClosure {} -> "TSO"
  StackClosure {} -> "STACK"
  BlockingQueueClosure {} -> "BLOCKING_QUEUE"
  BCOClosure {} -> "BCO"
  UnsupportedClosure {} -> "UNSUPPORTED"
  _ -> "CLOSURE"

renderProvLoc :: InfoProv -> String
renderProvLoc ip
  | not (null (ipSrcFile ip)) && not (null (ipSrcSpan ip)) = ipSrcFile ip <> ":" <> ipSrcSpan ip
  | not (null (ipLoc ip)) = ipLoc ip
  | otherwise = ipMod ip
