{ nixpkgs ? import <nixpkgs> {}}:
let
  packages = [
    "git-post-receive-types"
    "git-post-receive-hook"
    "git-post-receive-cat"
    "git-post-receive-exec"
    "git-post-receive-push"
    "git-post-receive-zre"
    "git-post-receive-zre2irc"
  ];

  inherit (import ./. { inherit nixpkgs; }) pkgs haskellPackages;
  hslib = nixpkgs.haskell.lib;
  extract-external-inputs = p:
    builtins.filter (dep: !(builtins.elem dep packages))
      (map (x: x.pname) (hslib.getHaskellBuildInputs haskellPackages.${p}));
  external-inputs = map (x: haskellPackages.${x}) (builtins.concatLists
    (map extract-external-inputs packages));
  package-envs = builtins.listToAttrs (map (p: {
    name = p;
    value = haskellPackages.${p}.env;
  }) packages);
in (haskellPackages.mkDerivation {
  pname = "git-post-receive";
  version = "0.0.0.0";
  libraryHaskellDepends = external-inputs;
  license = nixpkgs.stdenv.lib.licenses.asl20;
}).env // package-envs
