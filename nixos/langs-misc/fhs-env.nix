{ pkgs ? import <nixpkgs> {} }:
let
pp = import ./env-pin.nix {};
in
(pp.buildFHSUserEnv {
  name = "fhs-env";
  targetPkgs = pkgs: import ./env-packages.nix {};
  runScript = ./post-env.sh;
}).env
