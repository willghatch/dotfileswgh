{ pkgs ? import <nixpkgs> {} }:
let pp = import ./racket-env-pin.nix {};
in
let
#cp = pkgs;
cp = pp;
in
# I think the problem I was having before was probably using pkgs.mkShell
# instead of pp.mkShell here.  I think mkShell probably pulls in some core
# packages from the version of pkgs that hosts it.
(pp.buildFHSEnv {
  name = "racket-fhs-env";
  targetPkgs = pkgs: import ./racket-env-packages.nix {};
  runScript = ./racket-post-env.sh;
  profile = ''
    export LOCALE_ARCHIVE="${pp.glibcLocales}/lib/locale/locale-archive";
  '';
}).env
