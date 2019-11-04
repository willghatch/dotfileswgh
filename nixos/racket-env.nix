{ pkgs ? import <nixpkgs> {} }:
let pp = let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      # nixos-unstable as of 2019-11-04
      rev = "7827d3f4497ed722fedca57fd4d5ca1a65c38256";
      sha256 = "1131z88p359bq0djjmqah9i25cgxabrfkw4a4a7qq6j0d6djkfig";
    };
    in import pinnedPackages {};
    cp = pkgs;
in
pkgs.mkShell {
  buildInputs = [

    pp.cairo
    pp.fontconfig
    pp.glib
    pp.gmp
    pp.gtk2
    pp.libedit
    pp.libjpeg
    pp.libpng
    pp.mpfr
    pp.openssl
    pp.pango
    pp.poppler
    pp.readline
    pp.sqlite
    pp.coreutils
    pp.readline
    pp.libuuid
    # apparently `buildInputs` automatically adds `.dev`, but buildFHSUserEnv doesn't.
    pp.libuuid.dev

    pp.libffi

    pp.pkgconfig
    #pp.racket

    # for chez scheme
    #pp.cctools
    pp.ncurses
    pp.libiconv
    pp.xorg.libX11

    # build tools
    pp.gnumake
    pp.gcc
    # binutils has the `ar` command, and if `ar` is not present, a Racket build fails with an unhelpful message.
    pp.binutils


    # These are for convenience when entering the environment
    cp.bashInteractive
    cp.zsh
    cp.coreutils
    cp.emacs
    cp.man
    cp.gitAndTools.gitFull
    cp.tig
    cp.silver-searcher
    # poor man's gitk...
    cp.gitg
    cp.meld
    # for raco docs
    cp.firefox
    # for tic command
    cp.ncurses
    cp.which
    cp.less

    # These are for convenience in other non-racket things that I'm putting here for convenience...
    cp.gnum4
    cp.autoconf

  ];
}
