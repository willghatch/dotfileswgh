with import <nixpkgs> {};
let pp = let pinnedPackages = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      # nixos-18.09 as of 2019-01-14
      rev = "001b34abcb4d7f5cade707f7fd74fa27cbabb80b";
      sha256 = "1131z88p359bq0djjmqah9i25cgxabrfkw4a4a7qq6j0d6djkfig";
    };
    in import pinnedPackages {};
in
(pkgs.buildFHSUserEnv {
  name = "racket-dev-environment";
  # targetPkgs instead of buildInputs, and as function instead of list...
  targetPkgs = pkgs: [
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

    pp.pkgconfig
    #pp.racket

    # These are for convenience when entering the environment
    pkgs.bashInteractive
    pkgs.zsh
    pkgs.coreutils
    pkgs.emacs
    pkgs.man
    pkgs.git
    pkgs.tig
    pkgs.silver-searcher
    # poor man's gitk...
    pkgs.gitg
    pkgs.meld
    # for raco docs
    pkgs.firefox
    # for tic command
    pkgs.ncurses
  ];

}).env