{ pkgs ? import <nixpkgs> {} }:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-21.05 as of 2021-07-06
      rev = "fefb0df7d2ab2e1cabde7312238026dcdc972441";
      sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
    };
    in import pinnedPackages {}
