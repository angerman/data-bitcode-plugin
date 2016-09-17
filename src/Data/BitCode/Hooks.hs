{-# LANGUAGE NamedFieldPuns, LambdaCase #-}
module Data.BitCode.Hooks where

import qualified Data.BitCode.LLVM.Gen.Monad as Llvm
import Data.BitCode.LLVM.Gen (runLlvm, llvmCodeGen)

-- for outputfn
import Stream           (Stream)
import Module
import DynFlags
import Cmm
-- for phaseInputExt
import DriverPhases (Phase(..))
import DriverPipeline
import SysTools
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

--------------------------------------------------------------------------------
-- Plugin Hook
outputFn :: (DynFlags -> Module -> ModLocation -> FilePath -> Stream IO RawCmmGroup () -> [UnitId] -> IO ())
         -> DynFlags -> Module -> ModLocation -> FilePath -> Stream IO RawCmmGroup () -> [UnitId] -> IO ()
outputFn super dflags mod mloc fp cmm_stream pkg_deps = runLlvm dflags fp $ llvmCodeGen (Llvm.liftStream cmm_stream)

phaseInputExt :: (Phase -> String) -> Phase -> String
phaseInputExt super = \case
  LlvmOpt    -> "bc"
  (As _)     -> "bc"
  p -> super p

phaseHook :: (PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath))
          -> PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath)
phaseHook super phase input_fn dflags = case phase of
  -- Optimization phase
  (RealPhase LlvmOpt) -> do
    -- we skip Llc and Mangle;
    -- and go directly from the
    -- optimizer to the linker.
    let next_phase = As False
    output_fn <- phaseOutputFilename next_phase
    liftIO $ SysTools.runLlvmOpt dflags
      ([ SysTools.FileOption "" input_fn,
         SysTools.Option "-o",
         SysTools.FileOption "" output_fn]
       ++ [SysTools.Option "-mem2reg"])

    return (RealPhase next_phase, output_fn)

  -- Assembly phase
  (RealPhase (As with_cpp)) -> do

    next_phase <- maybeMergeStub
    output_fn <- phaseOutputFilename next_phase

    liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

    liftIO $ SysTools.runClang dflags
      ([ SysTools.Option "-c"
       , SysTools.FileOption "" input_fn
       , SysTools.Option "-o"
       , SysTools.FileOption "" output_fn
       ])
    return (RealPhase next_phase, output_fn)

  _ -> super phase input_fn dflags

maybeMergeStub :: CompPipeline Phase
maybeMergeStub
 = do
     PipeState{maybe_stub_o} <- getPipeState
     if isJust maybe_stub_o then return MergeStub else return StopLn
