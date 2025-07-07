{ pkgs ? import <nixpkgs> {} }:
let
pp = import ./env-pin.nix {};
in
(pp.buildFHSEnv {
  name = "fhs-env";
  targetPkgs = pkgs: import ./env-packages.nix {};
  runScript = ./post-env.sh;
}).env
