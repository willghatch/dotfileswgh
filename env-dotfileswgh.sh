# This is the necessary environment that my various dotfiles reference

# $WGHHOME is set so that I can su to other accounts and keep using my settings
if [ -z "$WGHHOME" ]; then
    export WGHHOME=$HOME
fi
export DOTFILESWGH=$WGHHOME/dotfileswgh
PATH=$DOFTILESWGH/commands:$PATH

