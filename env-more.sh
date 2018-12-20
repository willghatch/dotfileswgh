# Requires $WGHHOME and $DOTFILESWGH be set

export HROOT=$WGHHOME/hroot

export RACKET_PKG_DEV_PATH=$WGHHOME/mk/racket-pkgdev/racket/bin:$WGHHOME/mk/racket-pkgdev/bin:$WGHHOME/.racket/pkgdev/bin
export RACKET_CORE_DEV_PATH=$WGHHOME/mk/racket-coredev/racket/bin

# CURRENT_DEV_PATH may be set by various commands.
export PATH=/$CURRENT_DEV_PATH:$DOTFILESWGH/pri/dotlocal/commands:$DOTFILESWGH/dotlocal/commands:$WGHHOME/rootgit-dotfiles/commands:/rootgit/bin.rootgit:/rootgit/tools.rootgit:$DOTFILESWGH/pri/commands:$DOTFILESWGH/pri/commands/aliases:$DOTFILESWGH/commands:$DOTFILESWGH/commands/aliases:$DOTFILESWGH/external/misc/rootgit-tools:$DOTFILESWGH/dotlocal/racket-pkgs-bin:$DOTFILESWGH/dotlocal/racket-bin:$WGHHOME/.cabal/bin:$HROOT/bin:$HROOT/usr/bin:$HROOT/usr/local/bin:$WGHHOME/.local/bin:$WGHHOME/bin:$PATH:/usr/bin:/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin

# XDG basedir spec
# I'm putting the defaults all here as reference
# Having the read-only path to search for config and data is *awesome*,
# but unfortunately most things ignore them and only use data/config_home.
#
# writable data location
## TODO -- add dotfile dirs to CONFIG/DATA_DIRS, but not the ones that are symlinked
#XDG_DATA_HOME=$HOME/.local/share
# read-only but searched data locations
#XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_DATA_DIRS=$DOTFILESWGH/local/share:$XDG_DATA_DIRS:/usr/local/share:/usr/share
# writable config location
#XDG_CONFIG_HOME=$HOME/.config
# read-only but searched config locations
export XDG_CONFIG_DIRS=$DOTFILESWGH/pri/xdg-config-ro:$DOTFILESWGH/xdg-config-ro:$WGHHOME/rootgit-dotfiles/xdg-config-ro:$XDG_CONFIG_DIRS
#XDG_CONFIG_DIRS=/etc/xdg
# writable cache dir
export XDG_CACHE_HOME=$HOME/.cache


export MANWIDTH=80
export QT_STYLE_OVERRIDE=gtk

export ZDOTDIR=$DOTFILESWGH/zsh

export EDITOR=premacs-use-or-create-t
#export ALTERNATE_EDITOR=v
#export PAGER=premacs-pager
export PAGER=less

export XDG_DOWNLOAD_DIR="$HOME/dl"
export VLAUNCHRC="$DOTFILESWGH/vlaunchrc"

export GREP_COLOR="1;32"

export TODOMAN_CONFIG=$DOTFILESWGH/pri/todoman.conf
export VDIRSYNCER_CONFIG=$DOTFILESWGH/pri/vdirsyncer.conf
export KHARD_CONFIG=$DOTFILESWGH/pri/khard.conf
