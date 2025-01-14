# This is the necessary environment that my various dotfiles reference

if [ -z "$DOTFILESWGH" ]; then
    if [[ -d "$HOME/dotfileswgh" ]]; then
        export DOTFILESWGH="$HOME/dotfileswgh"
    elif [[ -d /rootgit/base.rootgit/dotfileswgh ]]; then
        export DOTFILESWGH=/rootgit/base.rootgit/dotfileswgh
    fi
fi

export DOTFILESWGH="${DOTFILESWGH:-$HOME/dotfileswgh}"

#PATH=$DOTFILESWGH/commands:$DOTFILESWGH/commands/aliases:$PATH

