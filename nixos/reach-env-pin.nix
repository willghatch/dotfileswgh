{ pkgs ? import <nixpkgs> {} }:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-unstable as of 2022-01-19
      rev = "d5dae6569ea9952f1ae4e727946d93a71c507821";
      sha256 = "1w4wch077d2wgrdaya8y1pa34rzvh26xns6zgzr4kmlfd5g2rhwd";
    };
    in import pinnedPackages {}
