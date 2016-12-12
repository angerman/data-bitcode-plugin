{ nixpkgs ? import <nixpkgs> {} }:
let
  # get our custom plugin enabled ghc
  ghc = (nixpkgs.callPackage ./ghc {}).pluginGhc;
  # get the plugin expression
  data-bitcode-plugin = ghc.callPackage ./default.nix {};
  # setup a ghc with the plugin in the package database.
  myGhc = ghc.ghcWithPackages (pkgs: [data-bitcode-plugin]);
in
# Create an empty derivation, that has the
# custom ghc as well as llvm available.
nixpkgs.pkgs.stdenv.mkDerivation {
  name = "data-bitcode-plugin-env";
  buildInputs = [ myGhc nixpkgs.pkgs.llvm_39 ];
}
