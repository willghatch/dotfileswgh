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

  # Here is a blog post that discusses using Stack on NixOS in a shell environment: https://vaibhavsagar.com/blog/2018/03/17/faking-non-nixos-stack/

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


  (pp.callPackage ./haskell-language-server-for-reach.nix {})

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
