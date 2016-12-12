{-# LANGUAGE NamedFieldPuns, LambdaCase #-}
module Data.BitCode.Hooks where

import qualified Data.BitCode.LLVM.Gen.Monad as Llvm
import Data.BitCode.LLVM.Gen (runLlvm, llvmCodeGen)

-- for outputfn
import Stream           (Stream)
import Module
import DynFlags
import Cmm
import Plugins (CommandLineOption)
-- for phaseInputExt
import DriverPhases (Phase(..))
import DriverPipeline
import SysTools
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Stream

type Hook' a = a -> a

--------------------------------------------------------------------------------
-- Plugin Hook
outputFn :: [CommandLineOption] -> Hook' (DynFlags -> Module -> ModLocation -> FilePath -> Stream IO RawCmmGroup () -> [InstalledUnitId] -> IO ())
outputFn opts super dflags mod mloc fp cmm_stream pkg_deps = do
  elms <- Stream.collect cmm_stream
  runLlvm opts dflags fp $ llvmCodeGen (Llvm.liftStream cmm_stream)
  -- super reinitGlobals dflags mod mloc fp cmm_stream pkg_deps

phaseInputExt :: Hook' (Phase -> String)
phaseInputExt super = \case
  LlvmOpt    -> "bc"
  (As _)     -> "bc"
  p -> super p

phaseHook :: Hook' (PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath))
phaseHook super phase input_fn dflags = case phase of
  -- ensure we use clang to compile C code. And also make sure it emits bitcode.
  -- the default impl will pass -S to generate assembly, luckily that option is
  -- ignored if we pass -emit-llvm first. Yes. This. Is. A. Hack!
  (RealPhase Cc) -> let (p,args0) = pgm_c dflags
                    in super phase input_fn (dflags { settings = (settings dflags) { sPgm_c = ("clang", (Option "-emit-llvm"):args0) } })
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
