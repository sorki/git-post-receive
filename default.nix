{ nixpkgs ? import <nixpkgs> {} }: let
  overlay = import ./overlay.nix;
  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then nixpkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };
in {
  haskellPackages =
    nixpkgs.haskellPackages.override overrideHaskellPackages;
  inherit nixpkgs;
}
