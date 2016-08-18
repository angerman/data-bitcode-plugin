# A ghc llvm backend plugin

This is a plugin that hooks into ghc's compiliation pipeline to produce bitcode
as a result.

## Running it

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
