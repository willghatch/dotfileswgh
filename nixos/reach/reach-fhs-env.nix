{ pkgs ? import <nixpkgs> {} }:
let pp = import ./reach-env-pin.nix {};
in
(pp.buildFHSUserEnv {
  name = "reach-fhs-env";
  targetPkgs = pkgs: import ./reach-env-packages.nix {};
  runScript = ./reach-post-env.sh;
}).env
