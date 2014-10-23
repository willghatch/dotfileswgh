PS1_time='%F{cyan}%T'
HOSTCOLOR="%F{yellow}"
PS1_user_host='%F{green}[%B%n%b${HOSTCOLOR}@%B%m%b%F{green}]'
PS1_dir='%F{blue}%B%~'
PS1_cmd_stat='%b%F{cyan}<%(?,%F{green}%?,%F{red}%?)%F{cyan}>'
PS1_end='%F{default}%# '
PS1_endl='
'
PS1_batt_state="\$(batt_state.bash)"
VIMODE="I"
PS1_vi_state="\$VIMODE"
getGitBranchForPrompt() {
    branch=$(git branch 2>/dev/null | fgrep '*')
    if [ "$branch" = "* master" ]
    then
        branch="%F{grey}[%F{white}${branch:2}%F{grey}]"
    elif [ -n "$branch" ]
    then
        branch="%F{grey}[%F{red}${branch:2}%F{grey}]"
    fi
    echo $branch
}
PS1_git_branch="\$(getGitBranchForPrompt)"

zle-line-init zle-keymap-select() {
    VIMODE="${${KEYMAP/vicmd/N}/(main|viins)/I}"
    if [ "$VIMODE" = "opp" ]
    then
        VIMODE="O"
    elif [ "$VIMODE" = "vivis" ]
    then
        VIMODE="V"
    fi
    if [ "$VIMODE" = "N" ]
    then
        VIMODE="%K{cyan}%F{black}$VIMODE%k"
    elif [ "$VIMODE" = "I" ]
    then
        VIMODE="%K{magenta}%F{black}$VIMODE%k"
    elif [ "$VIMODE" = "O" ]
    then
        VIMODE="%K{yellow}%F{black}$VIMODE%k"
    elif [ "$VIMODE" = "V" ]
    then
        VIMODE="%K{green}%F{black}$VIMODE%k"
    else
        VIMODE="%K{white}%F{black}$VIMODE%k"
    fi
    PS1="${PS1_time} ${PS1_user_host} ${PS1_git_branch} ${PS1_dir} ${PS1_cmd_stat}${PS1_endl}${PS1_vi_state}${PS1_end}"
    zle reset-prompt
}
zle -N zle-keymap-select
zle -N zle-line-init
