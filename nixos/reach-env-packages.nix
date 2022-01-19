{ pkgs ? import <nixpkgs> {} }:
let pp = import ./reach-env-pin.nix {};
in
let
#cp = pkgs;
cp = pp;
in [

  pp.stack
  # The NixOS 21.11 version is older than required for Reach, but current unstable has a new enough version (as of 2022-01-17).
  pp.z3
  # Mo is not packages in Nixpkgs.  I could contribute this perhaps.  It's a pretty simple package.
  (pp.callPackage ./mo.nix {})
  # `solc` - solidity compiler.
  # As of 2022-01-17 the version in NixOS, including unstable, is only 0.8.2, while Reach requires 0.8.9.
  #pp.solc
  # On github they distribute a pre-built static binary for Linux, but it doesn't seem to want to execute.  I'm not sure what the missing piece is.
  #(pp.callPackage ./solc-static-linux.nix {})
  (pp.callPackage ./solc-updated.nix {})


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
