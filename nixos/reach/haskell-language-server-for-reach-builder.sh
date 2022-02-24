
# this is a bash script, but it does not use the #!.

# This sets up an environment, including putting coreutils in PATH
source $stdenv/setup
# environment includes:
# $out -- path to where output must go
# $src -- path to where source lives
# $PWD and $TMP are temporary directories (maybe the same)
# $NIX_BUILD_CORES and $NIX_STORE are available

# TODO - how can I inherit this from the main build file?  An easy way would be to put this script inline in the definition file.
VERSION_NUMBER="$1"

mkdir -p $out/bin
cp $src $out/bin/haskell-language-server.gz
gunzip $out/bin/haskell-language-server.gz
chmod +x $out/bin/haskell-language-server
# emacs expects this name, so let's put it there.
ln -s haskell-language-server $out/bin/haskell-language-server-wrapper

