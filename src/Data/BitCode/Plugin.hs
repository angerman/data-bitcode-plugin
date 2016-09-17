module Data.BitCode.Plugin (plugin) where

import DynFlags
import Plugins
import Hooks

-- for outputfn
import Stream           (Stream)
import Module

import qualified Stream
-- debugging
import Debug.Trace
import Cmm
import Cmm.Pretty

import DynFlags

import Data.BitCode.Hooks (outputFn, phaseInputExt, phaseHook)

--------------------------------------------------------------------------------
-- WARN: Do *not* define any symbols here that might conflict with GHC Symbols
--       put them into a different module! Otherwise hell may freeze over.
--
-- e.g.
--       ghc: panic! (the 'impossible' happened)
--         (GHC version 8.0.1 for x86_64-apple-darwin):
--               tyThingTyCon Identifier ‘encodeFloat’
--

-- Utility fn to install the codeOutputHook in the DynFlags Hooks.
-- NOTE: Not running traceShow here, results in a panic :(
--       Running seq, doesn't help either :(
installHook :: [CommandLineOption] -> DynFlags -> DynFlags
installHook _ dflags = traceShow "..." $ dflags { hooks = addHook (hooks dflags)
                                                -- this is essentially a hack.
                                                -- we piggyback on the Llvm pipeline.
                                                -- we could alternatively provide -fllvm.
                                                -- TODO: fix DriverPipeline.hs to not
                                                --       depend on hscPostBackendPhase.
                                                --       And turn that into something more
                                                --       generic.
                                                , hscTarget = HscLlvm
                                                }
  where
    addHook h = h { codeOutputHook    = Just outputFn
                  , phaseInputExtHook = Just phaseInputExt
                  , runPhaseHook      = Just phaseHook
                  }

-- We are a plugin!
plugin :: Plugin
plugin = defaultPlugin { updateDynFlags = installHook }

--------------------------------------------------------------------------------

-- outputPpFn :: DynFlags -> Module -> ModLocation -> FilePath -> Stream IO RawCmmGroup () -> IO ()
-- outputPpFn dflags mod loc filenm cmm_stream = do
--   putStrLn $ "Custom code gen: " ++ filenm
--   -- using showSDoc dflags, results in a panic :(
--   -- let llvmStream = Stream.mapM (mapM_ ppDecl) cmm_stream
--   -- _ <- Stream.collect llvmStream
--   return ()

