# This is the necessary environment that my various dotfiles reference

# $WGHHOME is set so that I can su to other accounts and keep using my settings
if [ -z "$WGHHOME" ]; then
    if [ -d /home/wgh ]; then
        export WGHHOME=/home/wgh
    else
        export WGHHOME=$HOME
    fi
fi
export DOTFILESWGH="${DOTFILESWGH:-$WGHHOME/dotfileswgh}"
#PATH=$DOTFILESWGH/commands:$DOTFILESWGH/commands/aliases:$PATH

