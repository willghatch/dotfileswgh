{stdenv, lib, pkgs, fetchurl}:

stdenv.mkDerivation rec {
  name = "mo-${version}";
  version = "2.3.3";
  src = fetchurl {
    url = "https://github.com/tests-always-included/mo/archive/refs/tags/2.3.3.tar.gz";
    sha256 = "bbf3b2002724e8f8f58418c31b411d37de697c2c7241e4500939eea0673997d2";
  };
  builder = "{bash}/bin/bash";
  args = [./mo-builder.sh "${version}"];

  meta = with lib; {
    #maintainers = [maintainers.willghatch];
    description = "Mo - Mustache Templates in Bash";
    homepage = https://github.com/tests-always-included/mo;
    # TODO - modified MIT license
    #license = licenses.mit;
    platforms = platforms.unix;
  };
}

