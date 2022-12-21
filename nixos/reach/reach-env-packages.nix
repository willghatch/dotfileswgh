{ pkgs ? import <nixpkgs> {} }:
let ncursesOverlay =
  # stack GHC complains if libtinfo from ncurses doesn't have this flag
  (self: super: {ncurses5 = super.ncurses5.overrideAttrs (oldAttrs: {configureFlags = oldAttrs.configureFlags ++ ["--with-versioned-syms"];});});
in
let pp = import ./reach-env-pin.nix { overlays = [ncursesOverlay];};
in
let hp =
  #import (pkgs.fetchFromGitHub {
  #    owner = "NixOS";
  #    repo = "nixpkgs";
  #    # nixos-unstable as of 2022-10-17
  #    rev = "104e8082de1b20f9d0e1f05b1028795ed0e0e4bc";
  #    sha256 = "sha256-cPe3F7CtnxU9YbJpc3Adl4d9kX+turqTv5FxM98i8vg=";
  #  }) {overlays = [ncursesOverlay];};
  pp;
in
let
#cp = pkgs;
cp = pp;
in [

  # Here is a blog post that discusses using Stack on NixOS in a shell environment: https://vaibhavsagar.com/blog/2018/03/17/faking-non-nixos-stack/

  # For stack to work normally (not trying to leverage nix) in this nix-shell environment, you need some configuration in ~/.stack/config.yaml.  Specifically:
  # ghc-build: standard
  # nix:
  #  enable: false

  hp.stack
  # GHC dependencies for stack
  # {{{
  hp.gcc
  hp.gnumake
  hp.libffi
  hp.libffi.dev
  hp.zlib
  hp.zlib.dev
  hp.gmp
  hp.gmp.dev
  # ncurses has libtinfo
  hp.ncurses5
  hp.ncurses5.dev
  hp.binutils
  hp.pkgconfig
  # }}}



  # The NixOS 21.11 version is older than required for Reach, but current unstable has a new enough version (as of 2022-01-17).
  #pp.z3
  (pp.callPackage ./z3-updated.nix {}).z3_4_8_17
  #(pp.callPackage ./z3-updated.nix {}).z3_4_11_0
  # Mo is not packaged in Nixpkgs.  I could contribute this perhaps.  It's a pretty simple package.
  (pp.callPackage ./mo.nix {})
  # `solc` - solidity compiler.
  #(pp.callPackage ./solc-updated.nix {})
  #pp.solc
  #(pp.callPackage ./solc-updated.nix {z3 = (pp.callPackage ./z3-updated.nix {}).z3_4_8_17;})
  (pp.callPackage ./solc-updated.nix {z3 = (pp.callPackage ./z3-updated.nix {}).z3_4_11_0;})

  pp.docker
  pp.docker-compose
  pp.curl
  pp.which
  pp.wget


  ### The below are not necessary for building Reach, but are useful.

  # stuff for scripts in scripts dir
  pp.jq
  pp.bat

  # for visualizations with --intermediate-files compilation
  pp.graphviz

  (pp.haskell-language-server.override {supportedGhcVersions = ["902"];})
  # Let's use the updated package from newer nixpkgs, but build it with pinned packages.
  #(pp.callPackage (pp.fetchurl {
  #  url = "https://raw.githubusercontent.com/NixOS/nixpkgs/3d7435c638baffaa826b85459df0fff47f12317d/pkgs/development/tools/haskell/haskell-language-server/withWrapper.nix";
  #  sha256 = "1b8ddvw0i10cnr6vf2sim5dixhkyldf7676qa4kavhdwlsznjkig";
  #}) { supportedGhcVersions = ["902"]; })


  (pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-unstable as of 2022-06-17
      rev = "3d7435c638baffaa826b85459df0fff47f12317d";
      sha256 = "19ahb9ww3r9p1ip9aj7f6rs53qyppbalz3997pzx2vv0aiaq3lz3";
    })

  # For typescript language server
  pp.nodejs
  pp.nodePackages.npm
  pp.nodePackages.typescript-language-server
  pp.yarn
  pp.postgresql
  pp.nodePackages.prisma
  pp.openssl

  # for `make js-build` in the js/stdlib dir
  # TODO - this is probably too old, but I'll just go into the docker container to see things for now.
  pp.nodePackages.typescript

  # EVM disassembler
  pp.evmdis


  # This makes locales and unicode work.
  # No nix-shell environment should go without it.
  cp.glibcLocales




  # These are just for convenience when entering the environment, because I like to have tools available
  cp.bashInteractive
  cp.zsh
  cp.coreutils
  cp.vim
  cp.emacs-nox
  #cp.man
  cp.gitAndTools.gitFull
  cp.tig
  (cp.callPackage ../misc/ripgrep-renamed.nix {})
  cp.fzf


]
