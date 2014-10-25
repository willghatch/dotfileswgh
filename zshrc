#!/usr/bin/zsh


completer_default_setup(){
    zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z} m:{b,m,w,v,h,t,n,g,c,r,-}={0,1,2,3,4,5,6,7,8,9,_}'
}
COMPLETER_DEFAULT_SETUP=completer_default_setup
VZSH_REMAP_KEYS_P=true

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

source $HOME/dotfileswgh/external/zsh/antigen/antigen.zsh
antigen use oh-my-zsh
antigen bundles <<EOBUNDLES
    git
    zsh-users/zaw
    zsh-users/zsh-history-substring-search
    https://github.com/hchbaw/opp.zsh.git
    # TODO - get opp's sub-pieces working (surround, between)
    https://github.com/alfredodeza/zsh-plugins.git vi #vi visual
    wd
    zsh-users/zsh-syntax-highlighting
EOBUNDLES

source $HOME/dotfileswgh/bazsh/common.sh

if [[ -d ~/vzsh ]]; then
    antigen bundle ~/vzsh --no-local-clone
else
    antigen bundle https://github.com/willghatch/vzsh.git
fi

#antigen theme vzsh

antigen apply

fpath=($fpath $HROOT/build/zsh-completions/src)

# Highlighter plugin config
# Highlighters main
ZSH_HIGHLIGHT_STYLES[default]="none"
ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=red,bold"
ZSH_HIGHLIGHT_STYLES[path]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[path_approx]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[path_prefix]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[globbing]="fg=blue,bold,strikethrough"
ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=yellow"
ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[precommand]="fg=green"
ZSH_HIGHLIGHT_STYLES[command]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[hashed-command]="fg=green"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[alias]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[assign]="fg=white,bold"
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]="fg=magenta"
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=yellow"
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]="fg=magenta,bold"
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]="fg=red"
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]="fg=red"
# Highlighters root
ZSH_HIGHLIGHT_STYLES[root]="standout"
# Highlighters cursor
ZSH_HIGHLIGHT_STYLES[cursor]="standout"
# Highlighters brackets
ZSH_HIGHLIGHT_STYLES[bracket-level-1]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-2]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-3]="fg=magenta,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-4]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-5]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[bracket-error]="fg=red,bold"
ZSH_HIGHLIGHT_STYLES[cursor-matchingbracket]="standout"
# Highlighters pattern
# TODO -- use this one, because it looks cool and helpful, but requires more setup


# include history substring search commands
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindToMaps '^[c' execute-named-cmd $(bindkey -l)
bindkey -r viins '\e'
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode


tf=$DOTFILESLOCALDIR/bazsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESLOCALDIR/zshrc
if [ -f "$tf" ]
then
    source $tf
fi
unset tf

if [ -x ~/vscripts/motd.sh ]
then
    ~/vscripts/motd.sh
fi

