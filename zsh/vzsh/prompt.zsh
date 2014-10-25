# Prompt setup.  TODO - look at zsh's built in prompt theme framework

PS1_time='%F{cyan}%T'
PS1_hostcolor="%F{yellow}"
PS1_user_host='%F{green}[%B%n%b${PS1_hostcolor}@%B%m%b%F{green}]'
PS1_dir='%F{blue}%B%~'
PS1_cmd_stat='%b%F{cyan}<%(?,%F{green}%?,%F{red}%?)%F{cyan}>'
PS1_hist='%F{yellow}%h'
PS1_end='%F{default}%# '
PS1_endl='
'
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

update-prompt() {
    local VIMODE=$CUR_KEYMAP
    if [[ "$VIMODE" = "viins" || "$VIMODE" = "emacs" || "$VIMODE" = "main" ]]
    then
        VIMODE="%K{magenta}%F{black}I%k"
    elif [[ "$VIMODE" = "vicmd" ]]
    then
        VIMODE="%K{blue}%F{black}O%k"
    elif [[ "$VIMODE" = "opp" ]]
    then
        VIMODE="%K{yellow}%F{black}O%k"
    elif [[ "$VIMODE" = "vivis" || "$VIMODE" = "vivli" ]]
    then
        VIMODE="%K{green}%F{black}V%k"
    else
        VIMODE="%K{white}%F{black}$VIMODE%k"
    fi

    # if the user has set up something custom, use it
    if [[ -n "$VZSH_PS1_UPDATE_FUNC" ]]; then
        $VZSH_PS1_UPDATE_FUNC
    else
        PS1="${PS1_time} ${PS1_user_host} ${PS1_git_branch} ${PS1_dir} ${PS1_cmd_stat}${PS1_endl}${PS1_vi_state}${PS1_hist}${PS1_end}"
    fi

    zle reset-prompt
}

ZLE_LINE_INIT_FUNCS=( $ZLE_LINE_INIT_FUNCS update-prompt )
ZLE_KEYMAP_SELECT_FUNCS=( $ZLE_KEYMAP_SELECT_FUNCS update-prompt )

