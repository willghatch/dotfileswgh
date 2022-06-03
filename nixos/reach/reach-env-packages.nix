{ pkgs ? import <nixpkgs> {} }:
let pp = import ./reach-env-pin.nix { overlays = [
  # stack GHC complains if libtinfo from ncurses doesn't have this flag
  (self: super: {ncurses5 = super.ncurses5.overrideAttrs (oldAttrs: {configureFlags = oldAttrs.configureFlags ++ ["--with-versioned-syms"];});})
  ];};
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
  # So let's copy the upstream package definition and just update it.  This is another likely candidate for actually sending to Nixpkgs.
  (pp.callPackage ./solc-updated.nix {})

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

  (pp.callPackage ./haskell-language-server-for-reach.nix {})

  # For typescript language server
  pp.nodejs
  pp.nodePackages.npm
  pp.nodePackages.typescript-language-server

  # for `make js-build` in the js/stdlib dir
  # TODO - this is probably too old, but I'll just go into the docker container to see things for now.
  pp.nodePackages.typescript


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


]
