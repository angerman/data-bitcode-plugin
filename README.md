# A ghc llvm backend plugin

This is a plugin that hooks into ghc's compiliation pipeline to produce bitcode
as a result.

## Running it

### via nix

See [data-bitcode-plugin-env](https://github.com/angerman/data-bitcode-plugin-env).

### via stack


Compile the plugin:
```
data-bitcode-plugin $ stack build
```

And compile a test module:
```
$ stack ghc -- Test.hs -fplugin Data.BitCode.Plugin -fllvm # -fllvm -keep-llvm-files are useful flags to compare it to the stock llvm backend.
```

This however requires that the ghc supports the extended plugin interface.

To see what the resulting llvm ir is, use the `llvm-dis` tool.

```
$ llvm-dis < File.bc | less
```

## Supported plugin flags

- `-dump-ast`: can be used to print the internal module representation for inspection.
- `-dump-module`: will write a `Data.Binary` serialized version of the internal bitcode representation into a `.bcbin` file. (to be used with `-keep-llvm-files`), that can be used to analyze potential performance issues (from the bitcode represetioatn onwards) in isolation.

Plugin arguments can be specified as `-fplugin-opt Data.BitCode.Plugin:-dump-ast` for example.

## Wishlist (TODOs)

- [ ] The EDSL does not support function level VSTs, and hence we do not have names for variable. This makes reading the produced
      llvm ir (via llvm-dis) quite hard (e.g. you do not know immediately what which register value is.)
- [ ] Fix the GHC plugin system, to parse files first before parsing commandline arguments. Otherwise `{-# ... #-}` is lost.
      e.g. this `{-# OPTIONS_GHC -fplugin Data.BitCode.Plugin #-}` is not picked up correctly.
- [ ] Properly use plugin arguments. (Would likely required the whole plugin to run through a reader.)
  - [ ] Allow things like -dump-typetable, -dump-valuetable, -dump-ast, -dump-statements
- [ ] Turn the plugin into an ExceptT and use catchE to find lower level exceptions.
- [ ] More pervasive use of `Text.PrettyPrint`
- [ ] Fix ExternallyVisible. We expose everything right now.

## GHC Compilation Pipeline:
```































in general keep the phases and platform data types. If
we wanted to introduce new phases or platforms, adding this
to GHC should be a minor issue.
                                                 
                                                
 class PipelineDriver where
   (1x) nextPhase :: DynFlags -> Phase -> Phase
   (3x) startPhase :: String -> Phase
   (6x) phaseInputExt :: Phase -> String

-- might want to customize the objish_suffixes, ..., dynlib_suffixes as well.

Maybe it's better to have hook functions for these three functions?
hookedNextPhase :: (DynFlags -> Phase -> Phase) -> DynFlags -> Phase -> Phase
hookedNextPhase super dflags phase
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                        
                                        
                                        








  +----------------------------+
  | runPhase (RealPhase CmmCpp)|
  +-----------|----------------+
              |
              v
  +--------------------------+   next_phase = hscPostBackendPhase dflags HsSrcFile hsc_lang   +-----------------------------------+
  | runPhase (RealPhase Cmm) |   output_fn  = phaseOutputFilename next_phase                  | hscPostBackendPhase               |
  +-----------|--------------+                - phaseOutputFilename stop_phase                | _ HsBootFile   _    = StopLn      |
              |                                                     output_spec               | _ HsigFile     _    = StopLn      |
              v                                                     src_basename              | d _            HscC = HCc         |
   +-------------------------+                                      dflags                    | d _            HscAsm = Splitter if SplitObj            
   | hscCompileCmmFile       |                                      next_phase                |                         As False else       
   +----------|--------------+                                      maybe_loc                 | d _            HscLlvm = LlvmOpt  |
              |                                 - getOutputFilename                           | d _            HscNothing = StopLn|
              |                                   nextPhase == stopPhase                      | d _            HscInterpreted = StopLn
              v                                    && Persistent - persistent_fn              +-----------------------------------+
  +---------------------------+                    && SpecificFile - outputfile dflags
  | codeOutput                |                   As && Opt_KeepS - persistent_fn     
  |                           |                   HCc && Opt_KeepHc - persistent_fn  
  |                           |                   LlvmOpt && Opt_KeepLlvmFiles - persistent_fn 
  |                           |                   ^
  +---------------------------+                   +- nextPhase  
                                                  Otherwise - newTempName dflags suffix

                                                  suffix Hcc       -> hcsuf
                                                         MergeStug -> osuf
                                                         StopLn    -> osuf
                                                         _other    -> phaseInputExt _other



                                                  
