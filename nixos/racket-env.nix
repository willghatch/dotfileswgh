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
stdenv.mkDerivation {
  name = "racket-dev-environment";
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

    pp.pkgconfig
    #pp.racket

    # These are for convenience when entering the environment
    bashInteractive
    zsh
    coreutils
    emacs
    man
  ];
}