
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
#
#

autoload -U colors && colors

setopt PROMPT_SUBST

umask 077

bazsh_common=$HOME/dotfileswgh/bazsh_common
if [ -f $bazsh_common ]
then
    source $bazsh_common
fi

PS1_time='%F{cyan}%T'
PS1_user_host='%F{green}[%B%n%b%F{yellow}@%B%m%b%F{green}]'
PS1_dir='%F{blue}%B%~'
PS1_cmd_stat='%b%F{cyan}<%F{red}%?%F{cyan}>'
PS1_end='%F{default}
%# '
PS1_batt_state="\$(batt_state.bash)" 

#PS1="%T [%n@%m] %~ %?%# "
#PS1='%F{cyan}%T %F{green}[%B%n%b%F{yellow}@%B%m%b%F{green}] %F{blue}%B%~ %b%F{cyan}<%F{red}%?%F{cyan}>%F{default}%#
#'
PS1="${PS1_time} ${PS1_user_host} ${PS1_dir} ${PS1_cmd_stat}${PS1_end}"


if [ -f $DOTFILESLOCALDIR/bazsh ]
then
    source $DOTFILESLOCALDIR/bazsh
fi

if [ -f $DOTFILESLOCALDIR/zshrc ]
then
    source $DOTFILESLOCALDIR/zshrc
fi

if [ -x ~/vscripts/motd.sh ]
then
    ~/vscripts/motd.sh
fi

