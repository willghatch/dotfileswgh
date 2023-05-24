{stdenv, ripgrep, bash}:

stdenv.mkDerivation rec {
  name = "ripgrep-renamed";
  buildInputs = [ ripgrep ];
  builder = "${bash}/bin/bash";
  args = [ ./ripgrep-renamed-builder.sh "${ripgrep}" ];
}

