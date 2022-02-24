{stdenv, ripgrep, bash}:

# Because “ripgrep” is the right name and short 1-3 character names should be reserved for user aliases.

stdenv.mkDerivation rec {
  name = "ripgrep-renamed";
  buildInputs = [ ripgrep ];
  builder = "${bash}/bin/bash";
  args = [ ./ripgrep-renamed-builder.sh "${ripgrep}" ];
}

