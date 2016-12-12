{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.callPackage ./ghc.nix {
  bootPkgs = nixpkgs.haskell.packages.ghc802;
  inherit (nixpkgs.haskellPackages) happy alex;
}
