{ nixpkgs ? import <nixpkgs> {} }:
let
 ghc = (nixpkgs.callPackage ./ghc {}).pluginGhc;
 data-bitcode = ghc.callPackage ../data-bitcode/data-bitcode.nix { };
 data-bitcode-llvm = ghc.callPackage ../data-bitcode-llvm/data-bitcode-llvm.nix { inherit data-bitcode; };
 data-bitcode-edsl = ghc.callPackage ../data-bitcode-edsl/data-bitcode-edsl.nix { inherit data-bitcode data-bitcode-llvm; };
in
 ghc.callPackage  ./data-bitcode-plugin.nix { inherit data-bitcode-edsl data-bitcode-llvm; }
