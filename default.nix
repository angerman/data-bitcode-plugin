{ nixpkgs ? import <nixpkgs> {} }:
let
 ghc = (nixpkgs.callPackage ./ghc {}).pluginGhc;
 data-bitcode = ghc.callPackage ./data-bitcode { };
 data-bitcode-llvm = ghc.callPackage ./data-bitcode-llvm { inherit data-bitcode; };
 data-bitcode-edsl = ghc.callPackage ./data-bitcode-edsl { inherit data-bitcode data-bitcode-llvm; };
in
 ghc.callPackage  ./data-bitcode-plugin.nix { inherit data-bitcode-edsl data-bitcode-llvm; }
