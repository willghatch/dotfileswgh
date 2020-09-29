{ pkgs ? import <nixpkgs> {} }:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      # nixos-20.03 as of 2020-04-27
      rev = "9137f05564eb50cc6f7042039aa9549a2e6e2340";
      sha256 = "0yh2fnywhiyhzrkdlccp0l3bmdrqj0y1gysln6x7xfl2zj3aij7z";
    };
    in import pinnedPackages {}
