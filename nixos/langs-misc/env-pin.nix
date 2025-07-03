{ pkgs ? import <nixpkgs> {} }:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-24.05 as of 2025-07-02
      rev = "628f7826b70243902032764585f36cbcc71ce246";
      hash = "sha256-DiO+E3lu1yj0sa/ovjmOtZ1VJ7wW7IJyUC9oTg41ZTA=";
    };
    in import pinnedPackages {}
