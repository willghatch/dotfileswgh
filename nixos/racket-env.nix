{ pkgs ? import <nixpkgs> {} }:
let pp = let pinnedPackages = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      # nixos-20.03 as of 2020-04-27
      rev = "9137f05564eb50cc6f7042039aa9549a2e6e2340";
      sha256 = "0yh2fnywhiyhzrkdlccp0l3bmdrqj0y1gysln6x7xfl2zj3aij7z";
    };
    in import pinnedPackages {};
in
let
#cp = pkgs;
cp = pp;
in
# I think the problem I was having before was probably using pkgs.mkShell
# instead of pp.mkShell here.  I think mkShell probably pulls in some core
# packages from the version of pkgs that hosts it.
pp.mkShell {
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
    cp.elinks
    # for tic command
    cp.ncurses
    cp.which
    cp.less
    # Note that my `premacs` emacs preloader plays poorly with `nix-shell`.  TODO - I should maybe add `premacs` commands to my dev environments so that it doesn't use a preloaded emacs.
    cp.par

    # for scribble --pdf
    #pp.texlive.combined.scheme-full

    # These are for convenience in other non-racket things that I'm putting here for convenience...
    cp.gnum4
    cp.autoconf

    # This makes locales and unicode work.
    cp.glibcLocales

  ];
}
