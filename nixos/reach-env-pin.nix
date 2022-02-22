{ pkgs ? import <nixpkgs> {} }:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-unstable as of 2022-01-19
      rev = "d5dae6569ea9952f1ae4e727946d93a71c507821";
      sha256 = "1w4wch077d2wgrdaya8y1pa34rzvh26xns6zgzr4kmlfd5g2rhwd";
      # testing...
      #rev = "77aa71f66fd05d9e7b7d1c084865d703a8008ab7";
      #sha256 = "1ir7y1aamdvxj9g849c4kc1ansbk3s6xpiwvpfycv2lxi5bnf0k7";
    };
    in import pinnedPackages {}
