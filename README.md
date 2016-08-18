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
$ stack ghc -- Test.hs -fplugin Plugin -fllvm # -fllvm -keep-llvm-files are useful flags to compare it to the stock llvm backend.
```

This however requires that the ghc supports the extended plugin interface.

To see what the resulting llvm ir is, use the `llvm-dis` tool.

```
$ llvm-dis < File.bc | less
```

## Wishlist (TODOs)

- [ ] Drop all *non*-live registers.  The ghc llvm backend does this already, we however don't, and always pass all registers.
- [ ] Trash registers.  The ghc llvm backend trashes registerst to assist in liveliness analysis; we however do not yet.
- [ ] The EDSL does not support function level VSTs, and hence we do not have names for variable. This makes reading the produced
      llvm ir (via llvm-dis) quite hard (e.g. you do not know immediately what which register value is.)
