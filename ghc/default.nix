{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.callPackage ./ghc.nix { }
