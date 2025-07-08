{ pkgs ? import <nixpkgs> {} }:
let
pp = import ./env-pin.nix {};
#cp = pkgs;
cp = pp;
in [

  pp.rustc
  pp.rustup
  pp.rustfmt
  pp.mrustc

  #pp.swift

  pp.clang
  pp.llvmPackages_latest.llvm
  pp.lld

  pp.gcc

  pp.pkg-config
  pp.autoconf
  pp.binutils

  pp.gnumake
  pp.cmake
  pp.ninja

  pp.python3

  pp.zlib
  pp.zlib.dev

  pp.gdb

  pp.nodejs
  pp.nodePackages.npm
  pp.nodePackages.yarn
  pp.nodePackages.typescript
  pp.nodePackages.typescript-language-server
  pp.nodePackages.prettier

  #pp.solc

  pp.openssl
  pp.openssl.dev


  ##################################################################

  # This makes locales and unicode work.
  cp.glibcLocales


  # These are for convenience when entering the environment
  cp.bashInteractive
  cp.ncurses5
  cp.zsh
  cp.coreutils
  cp.emacs
  cp.man
  cp.gitAndTools.gitFull
  cp.tig
  cp.aider-chat
  #(cp.callPackage ../misc/ripgrep-renamed.nix {})
  cp.ripgrep
  cp.silver-searcher
  # poor man's gitk...
  cp.gitg
  cp.meld
  cp.which
  cp.less
  # Note that my `premacs` emacs preloader plays poorly with `nix-shell`.  TODO - I should maybe add `premacs` commands to my dev environments so that it doesn't use a preloaded emacs.
  cp.par
  cp.xclip

]
