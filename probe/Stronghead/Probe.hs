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
  ) where

import Control.Concurrent.MVar (MVar, tryReadMVar)
import Control.Concurrent.STM (TVar, readTVarIO)
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe)
import GHC.Exts.Heap
  ( Box (..)
  , Closure
  , GenClosure (..)
  , asBox
  , getBoxedClosureData
  )
import GHC.Exts.Heap.InfoTable.Types (StgInfoTable (..))
import GHC.InfoProv (InfoProv (..), ipLoc, whereFrom)

import Stronghead.Agent (ProbeNode (..), registerProbe)

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
inspectBox :: ProbeOptions -> Box -> IO [ProbeNode]
inspectBox ProbeOptions {..} root = go [(0, -1, 0, root)] 1 []
  where
    go [] _ acc = pure (reverse acc)
    go ((nid, parent, depth, box@(Box raw)) : queue) nextId acc
      | length acc >= poMaxNodes = pure (reverse acc)
      | otherwise = do
          cl <- getBoxedClosureData box
          mProv <- whereFrom raw
          let (ty, name, bytes, isThunk, detail, kids) = describeClosure cl mProv
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
          go (queue ++ childItems) nextId' (node : acc)

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
-- Closure decoding

describeClosure :: Closure -> Maybe InfoProv -> (String, String, Int, Bool, String, [Box])
describeClosure cl mProv = case cl of
  ConstrClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (1 + length ptrArgs + length dataArgs)
        det =
          (if null modl then "" else modl <> ".")
            <> name
            <> (if null dataArgs then "" else " data=" <> show (take 4 dataArgs))
     in (ty, if null name then provName ty else name, sz, False, det, ptrArgs)
  ThunkClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (2 + length ptrArgs + length dataArgs)
        det = "ptrs=" <> show (length ptrArgs) <> " words=" <> show (length dataArgs)
     in (ty, provName ty, sz, True, det, ptrArgs)
  SelectorClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "THUNK_SELECTOR", 16, True, "selector thunk", [selectee])
  APClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (3 + length payload)
     in (ty, provName "AP", sz, True, "arity=" <> show arity <> " applied=" <> show n_args, fun : payload)
  APStackClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (2 + length payload)
     in (ty, provName "AP_STACK", sz, True, "stack_ptrs=" <> show (length payload), fun : payload)
  PAPClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (3 + length payload)
     in (ty, provName "PAP", sz, False, "arity=" <> show arity <> " applied=" <> show n_args, fun : payload)
  FunClosure {..} ->
    let ty = show (tipe info)
        sz = 8 * (1 + length ptrArgs + length dataArgs)
     in (ty, provName ty, sz, False, "free_ptrs=" <> show (length ptrArgs), ptrArgs)
  IndClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "IND", 16, False, "indirection", [indirectee])
  BlackholeClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "BLACKHOLE", 16, True, "under evaluation", [indirectee])
  ArrWordsClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "ARR_WORDS", 16 + fromIntegral bytes, False, show bytes <> " bytes", [])
  MutArrClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "MUT_ARR_PTRS", 16 + 8 * fromIntegral mccSize, False, show mccPtrs <> " ptrs", take 32 mccPayload)
  SmallMutArrClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "SMALL_MUT_ARR_PTRS", 16 + 8 * fromIntegral mccPtrs, False, show mccPtrs <> " ptrs", take 32 mccPayload)
  MVarClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "MVAR", 32, False, "MVar", [value])
  MutVarClosure {..} ->
    let ty = show (tipe info)
     in (ty, provName "MUT_VAR", 16, False, "IORef/MutVar", [var])
  IntClosure {..} ->
    ("INT", show intVal, 16, False, "Int", [])
  WordClosure {..} ->
    ("WORD", show wordVal, 16, False, "Word", [])
  Int64Closure {..} ->
    ("INT64", show int64Val, 16, False, "Int64", [])
  Word64Closure {..} ->
    ("WORD64", show word64Val, 16, False, "Word64", [])
  AddrClosure {..} ->
    ("ADDR", show addrVal, 16, False, "Addr", [])
  DoubleClosure {..} ->
    ("DOUBLE", show doubleVal, 16, False, "Double", [])
  FloatClosure {..} ->
    ("FLOAT", show floatVal, 16, False, "Float", [])
  UnknownTypeWordSizedPrimitive {..} ->
    ("PRIM_WORD", show wordVal, 8, False, "word-sized primitive", [])
  other ->
    let ty = closureTypeName other
     in (ty, provName ty, 16, False, "", [])
  where
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
