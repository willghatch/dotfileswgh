{ pkgs ? import <nixpkgs> {} }:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-24.05 as of 2024-11-18
      rev = "3c80acabe4eef35d8662733c7e058907fa33a65d";
      hash = "sha256-w9IIFo2Z5+JquKbDuWmkfKpApQ6lfvCcHbGvMkR3/uA=";
    };
    in import pinnedPackages {}
