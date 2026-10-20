# Requires $HOME and $DOTFILESWGH be set

export DOTFILESWGH="${DOTFILESWGH:-$HOME/dotfileswgh}"
export DOTFILESWGH_DOTLOCAL="${DOTFILESWGH_DOTLOCAL:-$HOME/dotfileswgh-dotlocal}"
export DOTFILESWGH_GHP="${DOTFILESWGH_GHP:-$HOME/dotfileswgh-ghp}"
export DOTFILESWGH_PRI="${DOTFILESWGH_PRI:-$HOME/dotfileswgh-pri}"
export DOTFILESWGH_PRI_DOTLOCAL="${DOTFILESWGH_PRI_DOTLOCAL:-$HOME/dotfileswgh-pri-dotlocal}"
export DOTFILESWGH_ROOTGIT="${DOTFILESWGH_ROOTGIT:-$HOME/rootgit-dotfiles}"

export HROOT=$HOME/hroot

export RACKET_NO_DEV_PATH=$DOTFILESWGH_DOTLOCAL/racket-pkgs-bin:$DOTFILESWGH_DOTLOCAL/racket-bin
export RACKET_PKG_DEV_PATH=$HOME/s/mk/racket-pkgdev/racket/bin:$HOME/s/mk/racket-pkgdev/bin:$HOME/.local/share/racket/pkgdev/bin
export RACKET_CORE_DEV_PATH=$HOME/s/mk/racket-coredev/racket/bin

DOTFILESWGH_COMMAND_PATH=$DOTFILESWGH_PRI_DOTLOCAL/commands:$DOTFILESWGH_DOTLOCAL/commands:$DOTFILESWGH_ROOTGIT/commands:/rootgit/bin.rootgit:/rootgit/base.rootgit/commands:/rootgit/tools.rootgit:$DOTFILESWGH_PRI/commands:$DOTFILESWGH_PRI/commands/aliases:$DOTFILESWGH_GHP/commands:$DOTFILESWGH/commands:$DOTFILESWGH/commands/aliases:$DOTFILESWGH/commands/iree

# DEV_TYPES: comma-separated list of dev-type namespaces to activate.
# For each type, prepend $MIXIN/commands/$type for every mixin.
if [ -n "$DEV_TYPES" ]; then
    _dfw_dt_path=""
    _dfw_remaining="$DEV_TYPES"
    while [ -n "$_dfw_remaining" ]; do
        _dfw_dt="${_dfw_remaining%%,*}"
        if [ "$_dfw_dt" = "$_dfw_remaining" ]; then
            _dfw_remaining=""
        else
            _dfw_remaining="${_dfw_remaining#*,}"
        fi
        _dfw_dt_path="$_dfw_dt_path:$DOTFILESWGH_PRI_DOTLOCAL/commands/$_dfw_dt"
        _dfw_dt_path="$_dfw_dt_path:$DOTFILESWGH_DOTLOCAL/commands/$_dfw_dt"
        _dfw_dt_path="$_dfw_dt_path:$DOTFILESWGH_ROOTGIT/commands/$_dfw_dt"
        _dfw_dt_path="$_dfw_dt_path:$DOTFILESWGH_PRI/commands/$_dfw_dt"
        _dfw_dt_path="$_dfw_dt_path:$DOTFILESWGH_GHP/commands/$_dfw_dt"
        _dfw_dt_path="$_dfw_dt_path:$DOTFILESWGH/commands/$_dfw_dt"
    done
    DOTFILESWGH_COMMAND_PATH="${_dfw_dt_path#:}:$DOTFILESWGH_COMMAND_PATH"
    unset _dfw_dt_path _dfw_remaining _dfw_dt
fi

HROOT_PATH=$HROOT/bin:$HROOT/usr/bin:$HROOT/usr/local/bin
COMMON_PATHS=/usr/bin:/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin
COMMON_HOME_PATHS=$HOME/.local/bin:$DOTFILESWGH_DOTLOCAL/npm/bin

# CURRENT_DEV_PATH may be set by various commands.
export PATH=${CURRENT_DEV_PATH:-$RACKET_NO_DEV_PATH}:$DOTFILESWGH_COMMAND_PATH:$HROOT_PATH:$PATH:$COMMON_HOME_PATHS

# XDG basedir spec
# I'm putting the defaults all here as reference
# Having the read-only path to search for config and data is *awesome*,
# but unfortunately most things ignore them and only use data/config_home.
#
# writable data location
## TODO -- add dotfile dirs to CONFIG/DATA_DIRS, but not the ones that are symlinked
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
# read-only but searched data locations
#XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_DATA_DIRS=$DOTFILESWGH/local/share:$XDG_DATA_DIRS:/usr/local/share:/usr/share
# writable config location
#XDG_CONFIG_HOME=$HOME/.config
# read-only but searched config locations
export XDG_CONFIG_DIRS=$DOTFILESWGH_PRI_DOTLOCAL/xdg-config-ro:$DOTFILESWGH_DOTLOCAL/xdg-config-ro:$DOTFILESWGH_ROOTGIT/xdg-config-ro:$DOTFILESWGH_PRI/xdg-config-ro:$DOTFILESWGH_GHP/xdg-config-ro:$DOTFILESWGH/xdg-config-ro:$XDG_CONFIG_DIRS
#XDG_CONFIG_DIRS=/etc/xdg
# writable cache dir
export XDG_CACHE_HOME=$HOME/.local/cache
# writable state location (histories, logs, etc)
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}


export MANWIDTH=80
# Use overstrikes so less applies the LESS_TERMCAP_* man-page colors.
export GROFF_NO_SGR=1

export ZDOTDIR=$DOTFILESWGH/zsh
# Programs without XDG config support, pointed at their XDG-style locations.
export INPUTRC=${XDG_CONFIG_HOME:-$HOME/.config}/readline/inputrc
export NPM_CONFIG_USERCONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/npm/npmrc
# The rest keep any value already set, eg. by a container launcher.
export NPM_CONFIG_CACHE=${NPM_CONFIG_CACHE:-$XDG_CACHE_HOME/npm}
export CARGO_HOME=${CARGO_HOME:-$XDG_DATA_HOME/cargo}
export RUSTUP_HOME=${RUSTUP_HOME:-$XDG_DATA_HOME/rustup}
export GRADLE_USER_HOME=${GRADLE_USER_HOME:-$XDG_DATA_HOME/gradle}
# dotnet makes a .dotnet directory inside DOTNET_CLI_HOME.
export DOTNET_CLI_HOME=${DOTNET_CLI_HOME:-$XDG_DATA_HOME/dotnet}
export NUGET_PACKAGES=${NUGET_PACKAGES:-$XDG_CACHE_HOME/NuGetPackages}
export ANDROID_USER_HOME=${ANDROID_USER_HOME:-$XDG_DATA_HOME/android}
export PLATFORMIO_CORE_DIR=${PLATFORMIO_CORE_DIR:-$XDG_DATA_HOME/platformio}
# libX11 compiled Compose table cache; only used if the directory exists (made in bazshrc).
export XCOMPOSECACHE=${XCOMPOSECACHE:-$XDG_CACHE_HOME/compose-cache}
# NOTE: apparently GTK reads $XDG_CONFIG_HOME/gtk-3.0/Compose (or 4.0 ...) and falls back to ~/.XCompose without reading XCOMPOSEFILE.  So maybe this one is not worth the hassle.  (Untested)
# libX11 compose file, instead of ~/.XCompose.
#export XCOMPOSEFILE=${XCOMPOSEFILE:-${XDG_CONFIG_HOME:-$HOME/.config}/XCompose}
export DOCKER_CONFIG=${DOCKER_CONFIG:-${XDG_CONFIG_HOME:-$HOME/.config}/docker}
export DVDCSS_CACHE=${DVDCSS_CACHE:-$XDG_CACHE_HOME/dvdcss}
# OpenSSL random seed file, instead of ~/.rnd.
export RANDFILE=${RANDFILE:-$XDG_STATE_HOME/openssl-rnd}
# PulseAudio clients fall back to ~/.pulse-cookie.
export PULSE_COOKIE=${PULSE_COOKIE:-${XDG_CONFIG_HOME:-$HOME/.config}/pulse/cookie}
export WGETRC=${WGETRC:-$DOTFILESWGH/wgetrc}
# less reads the lesskey source file directly, so no compiled ~/.less is needed.
export LESSKEYIN=${LESSKEYIN:-$DOTFILESWGH/lesskey}
export LESSHISTFILE=${LESSHISTFILE:-$XDG_STATE_HOME/less/history}
# Python >= 3.13
export PYTHON_HISTORY=${PYTHON_HISTORY:-$XDG_STATE_HOME/python/history}
# Compile terminfo entries here with `tic -o`, instead of the default ~/.terminfo.
# The trailing empty entry (when TERMINFO_DIRS was unset) means the system default.
export TERMINFO_DIRS=$XDG_DATA_HOME/terminfo:${TERMINFO_DIRS}

export EDITOR=e
#export EDITOR=premacs-use-or-create-t
#export ALTERNATE_EDITOR=v
#export PAGER=premacs-pager
export PAGER=less
export BROWSER=ffxd

export XDG_DOWNLOAD_DIR="$HOME/dl"
export VLAUNCHRC="$DOTFILESWGH/vlaunchrc"

export COLORTERM=truecolor
export GREP_COLOR="1;32"

export TODOMAN_CONFIG=$DOTFILESWGH_PRI/todoman.conf
export VDIRSYNCER_CONFIG=$DOTFILESWGH_PRI/vdirsyncer.conf
export KHARD_CONFIG=$DOTFILESWGH_PRI/khard.conf
export TSUEZ_TMUX_CONF=$DOTFILESWGH/tsuez-tmux.conf

export DOTFILESWGH_ENV_LOADED=true
