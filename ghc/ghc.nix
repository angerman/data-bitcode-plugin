{ nixpkgs ? import <nixpkgs> {}, ... } @ args:
let
  commonArgs = { bootPkgs = nixpkgs.haskell.packages.ghc802;
                 inherit (nixpkgs.haskellPackages) happy alex;
               };
  ghcBase = custom: nixpkgs.callPackage <nixpkgs/pkgs/development/haskell-modules> {
    ghc = nixpkgs.callPackage ./base.nix (args // commonArgs // custom);
    compilerConfig = nixpkgs.callPackage ./configuration-ghc.nix {};
  };
in
{ ghc81 = ghcBase {
    ghcVersion = "8.1.20161115";
    ghcRevision = "017d11e0a36866b05ace32ece1af11adf652a619";
    ghcSha256 = "1ryggmz961qd0fl50rkjjvi6g9azwla2vx9310a9nzjaj5x6ib4y";
  };
  pluginGhc = ghcBase {
    ghcVersion  = "8.1.20161211";
    ghcRevision = "490b9429a8ed3c55d17bf0964fb14582eb206a3d";
    ghcSha256   = "1mn5r9qa3b79h612c3r5isnxwpaxyh9im46n8lwm6a0ln948ff4x";
    ghcFlavour  = "quick";
    ghcDiffs    = [
      { diff = 2773; id = 9915; sha256 = "0cql8897by5hr3l7p5hag3fq9yhw8ic6l799mccdbmk7zfcdkfbd"; } # Make globals use sharedCAF
      { diff = 535;  id = 9918; sha256 = "1y87vvw5xdz20gnv9r3i3zbfbdj3vkkl3vzdc22nmy4pl3abqr4x"; } # Added a Plugin function to install hooks
    ];
  };
}
