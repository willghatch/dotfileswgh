{ pkgs ? import <nixpkgs> {}, overlays ? []}:
let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-unstable as of 2022-06-17
      #rev = "3d7435c638baffaa826b85459df0fff47f12317d";
      #sha256 = "19ahb9ww3r9p1ip9aj7f6rs53qyppbalz3997pzx2vv0aiaq3lz3";
      # nixos-unstable as of 2022-01-19
      #rev = "d5dae6569ea9952f1ae4e727946d93a71c507821";
      #sha256 = "1w4wch077d2wgrdaya8y1pa34rzvh26xns6zgzr4kmlfd5g2rhwd";
      # nixos-unstable as of 2022-10-17
      rev = "104e8082de1b20f9d0e1f05b1028795ed0e0e4bc";
      sha256 = "sha256-cPe3F7CtnxU9YbJpc3Adl4d9kX+turqTv5FxM98i8vg=";
    };
    in import pinnedPackages {overlays = overlays;}
