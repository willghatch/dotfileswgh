
# this is a bash script, but it does not use the #!.

# This sets up an environment, including putting coreutils in PATH
source $stdenv/setup
# environment includes:
# $out -- path to where output must go
# $src -- path to where source lives
# $PWD and $TMP are temporary directories (maybe the same)
# $NIX_BUILD_CORES and $NIX_STORE are available

pkgdir="$1"
linkup() {
    # $1 is the thing to copy, $2 (optional) is the new name
    local dir="$(dirname "$out/$1")"
    local base="$(basename "$out/$1")"
    mkdir -p "$dir"
    ln -s "${pkgdir}/$1" "$dir/${2:-$base}"
}

linkup bin/rg ripgrep
# Of course, the man page and other stuff will still say `rg` instead of `ripgrep`.
# Not a great situation.  But better than taking the name `rg` in the global unix command namespace!
linkup share/man/man1/rg.1.gz ripgrep.1.gz

# I want to copy over the zsh completion as well, but of course it depends on the name `rg` in various ways.  Dumb.
#mkdir -p $out/share/zsh/site-functions
#echo "#compdef ripgrep" >$out/share/zsh/site-functions/_ripgrep
#cat $pkgdir/share/zsh/site-functions/_rg | tail +2 | sed s/_rg/_ripgrep/ >>$out/share/zsh/site-functions/_ripgrep

