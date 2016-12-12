{ nixpkgs ? import <nixpkgs> {} }:
let
 myGhc = (nixpkgs.callPackage ./ghc {}).pluginGhc;
 myConfig = nixpkgs.callPackage ./ghc/configuration-ghc.nix {};
 ghc = nixpkgs.callPackage <nixpkgs/pkgs/development/haskell-modules> {
   ghc = myGhc;
   compilerConfig = myConfig;
 };
 data-bitcode = ghc.callPackage ../data-bitcode/data-bitcode.nix { };
 data-bitcode-llvm = ghc.callPackage ../data-bitcode-llvm/data-bitcode-llvm.nix { inherit data-bitcode; };
 data-bitcode-edsl = ghc.callPackage ../data-bitcode-edsl/data-bitcode-edsl.nix { inherit data-bitcode data-bitcode-llvm; };
in
 ghc.callPackage  ./data-bitcode-plugin.nix { inherit data-bitcode-edsl data-bitcode-llvm; }
