{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model Context Protocol (MCP) server for Stronghead built on 'mcp-server'.
module Stronghead.MCP.Server
  ( main
  , runServer
  , handleTool
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import MCP.Server
import MCP.Server.Derive
import System.Environment (getArgs)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

import Stronghead.MCP.Tools qualified as Tools
import Stronghead.MCP.Types (StrongheadTool (..), toolDescriptions)

-- | Handler mapping each 'StrongheadTool' to its semantic diagnostic engine.
handleTool :: ClientContext -> StrongheadTool -> IO ToolResult
handleTool _ctx tool = do
  res <- try @SomeException (dispatch tool)
  case res of
    Left ex -> pure $ toolError ("Unexpected error executing tool: " <> T.pack (show ex))
    Right (Left err) -> pure $ toolError err
    Right (Right txt) -> pure $ toolResult [ContentText txt]
  where
    dispatch = \case
      StrongheadDiagnoseLeak p topN threshold -> do
        Tools.diagnoseLeak (T.unpack p) (fromMaybe 5 topN) (fromMaybe 0.1 threshold)

      StrongheadInspectCore b dumpP mMod incStg -> do
        Tools.inspectCore b (fmap T.unpack dumpP) mMod (fromMaybe True incStg)

      StrongheadEventlogSummary p topThreads -> do
        Tools.eventlogSummary (T.unpack p) (fromMaybe 8 topThreads)

      StrongheadDiffRuns base cur mBaseDumps mCurDumps -> do
        Tools.diffRuns (T.unpack base) (T.unpack cur) (fmap T.unpack mBaseDumps) (fmap T.unpack mCurDumps)

      StrongheadRetainerPath mAddr mBand mTarget -> do
        r <- Tools.retainerPath mAddr mBand (fmap T.unpack mTarget)
        pure (Right r)

      StrongheadLiveControl cmd mHost mPort mText mTimeout -> do
        let host = maybe "127.0.0.1" T.unpack mHost
            port = fromMaybe 9100 mPort
            timeout = fromMaybe 3000 mTimeout
        Tools.liveControl cmd host port mText timeout

      StrongheadRecommendNextSteps issue mEntity mFile -> do
        r <- Tools.recommendNextSteps issue mEntity (fmap T.unpack mFile)
        pure (Right r)

      StrongheadGhciEval cmd mCwd mRepl -> do
        Tools.ghciEval cmd (fmap T.unpack mCwd) (fmap T.unpack mRepl)

      StrongheadRenderGraph p mBase mOut mTheme mTopN -> do
        Tools.renderGraph (T.unpack p) (fmap T.unpack mBase) (fmap T.unpack mOut) (fmap T.unpack mTheme) mTopN

-- | Run the MCP server over standard input and output (stdio).
runServer :: IO ()
runServer = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hPutStrLn stderr "Starting Stronghead MCP Server (powered by mcp-server)..."
  let tools = $(deriveToolHandlerWithDescription ''StrongheadTool 'handleTool toolDescriptions)
   in runMcpServerStdio
        McpServerInfo
          { serverName = "stronghead"
          , serverVersion = "0.1.0.0"
          , serverInstructions =
              """
              Stronghead MCP Server provides deep observability and semantic analysis into GHC programs:
              space leak diagnosis, Core/STG optimization inspection, eventlog runtime telemetry,
              empirical before/after diffing, live agent controls, and GC retainer graphs.
              """
          }
        noHandlers
          { tools = Just tools
          }

-- | Main entry point with CLI flags for inspecting available tools or starting the server.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printHelp
    ["-h"] -> printHelp
    ["--tools"] -> printTools
    ["tools"] -> printTools
    _ -> runServer

printHelp :: IO ()
printHelp =
  putStr
    """
    Stronghead MCP Server: Semantic GHC Profiling & Debugging Engine

    Usage: stronghead-mcp [OPTIONS]

    Options:
      --help, -h    Show this help message
      --tools       List available MCP tools and exit

    Standard MCP Stdio Mode:
      Run without arguments to start the MCP JSON-RPC stdio server.
      Compatible with Claude Desktop, Cursor, Zed, and Antigravity.
    """

printTools :: IO ()
printTools = do
  putStrLn "Available Stronghead MCP Tools (derived via mcp-server):\n"
  let tools =
        [ ("stronghead_diagnose_leak", "Pinpoint space leaks, calculate suspect scores, growth rates, and retention trees")
        , ("stronghead_inspect_core", "Inspect GHC Core and STG for worker-wrapper, unboxing, and let allocations")
        , ("stronghead_eventlog_summary", "Summarize runtime telemetry: GC pause percentiles, mutator frac, MVar contention")
        , ("stronghead_diff_runs", "Compare baseline vs current runs to empirically verify memory and latency improvements")
        , ("stronghead_retainer_path", "Trace shortest retainer path from GC roots via ghc-debug")
        , ("stronghead_live_control", "Trigger GCs, censuses, and toggle profiling in running programs via Stronghead.Agent")
        , ("stronghead_recommend_next_steps", "Prescriptive compiler and RTS flags for leaks, contention, and hot paths")
        , ("stronghead_ghci_eval", "Evaluate thunk trees and bindings in GHCi")
        , ("stronghead_render_graph", "Render publication-quality nano-ui heap graphs and PNG screenshots for issue reports and proof of fixes")
        ]
  forM_ tools $ \(name, desc) -> do
    putStrLn ("* " <> name <> ":")
    putStrLn ("  " <> desc <> "\n")
