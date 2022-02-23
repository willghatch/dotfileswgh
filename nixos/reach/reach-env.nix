{ pkgs ? import <nixpkgs> {} }:
let pp = import ./reach-env-pin.nix {};
in
pp.mkShell {
  buildInputs = import ./reach-env-packages.nix {};
}
