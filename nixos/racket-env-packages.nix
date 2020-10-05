{ pkgs ? import <nixpkgs> {} }:
let pp = import ./racket-env-pin.nix {};
in
let
#cp = pkgs;
cp = pp;
in [
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

]
