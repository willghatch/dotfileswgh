{ pkgs ? import <nixpkgs> {} }:
let pp = import ./reach-env-pin.nix {};
in
let
#cp = pkgs;
cp = pp;
nixos2105 = import (pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      # nixos-21.05 as of 2022-01-21
      rev = "0fd9ee1aa36ce865ad273f4f07fdc093adeb5c00";
      sha256 = "1mr2qgv5r2nmf6s3gqpcjj76zpsca6r61grzmqngwm0xlh958smx";
    }) {};


in [

  pp.stack
  # GHC dependencies for stack
  # {{{
  pp.gcc
  pp.gnumake
  pp.libffi
  pp.libffi.dev
  pp.zlib
  pp.zlib.dev
  pp.gmp
  pp.gmp.dev
  # ncurses has libtinfo
  #pp.ncurses
  #pp.ncurses.dev
  pp.ncurses5
  pp.ncurses5.dev
  pp.binutils
  pp.pkgconfig
  # }}}


  # The NixOS 21.11 version is older than required for Reach, but current unstable has a new enough version (as of 2022-01-17).
  pp.z3
  # Mo is not packaged in Nixpkgs.  I could contribute this perhaps.  It's a pretty simple package.
  (pp.callPackage ./mo.nix {})
  # `solc` - solidity compiler.
  # As of 2022-01-17 the version in NixOS, including unstable, is only 0.8.2, while Reach requires 0.8.9.
  #pp.solc
  # On github they distribute a pre-built static binary for Linux, but it doesn't seem to want to execute.  I'm not sure what the missing piece is.
  #(pp.callPackage ./solc-static-linux.nix {})
  (pp.callPackage ./solc-updated.nix {})
  pp.docker
  pp.docker-compose
  pp.curl
  pp.which
  pp.wget


  # These are for convenience when entering the environment
  cp.bashInteractive
  cp.zsh
  cp.coreutils
  cp.vim
  cp.emacs-nox
  #cp.man
  cp.gitAndTools.gitFull
  #cp.tig
  #cp.silver-searcher
  #cp.meld
  #cp.firefox
  #cp.elinks
  #cp.ncurses
  #cp.which
  #cp.less
  #cp.par
  #cp.hyperfine
  #cp.xclip

  # This makes locales and unicode work.
  cp.glibcLocales


]
