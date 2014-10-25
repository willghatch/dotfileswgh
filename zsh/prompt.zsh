PS1_time='%F{cyan}%T'
HOSTCOLOR="%F{yellow}"
PS1_user_host='%F{green}[%B%n%b${HOSTCOLOR}@%B%m%b%F{green}]'
PS1_dir='%F{blue}%B%~'
PS1_cmd_stat='%b%F{cyan}<%(?,%F{green}%?,%F{red}%?)%F{cyan}>'
PS1_hist='%F{yellow}%h'
PS1_end='%F{default}%# '
PS1_endl='
'
PS1_batt_state="\$(batt_state.bash)"
CUR_KEYMAP="main"
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
    local VIMODE=$KEYMAP
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
    PS1="${PS1_time} ${PS1_user_host} ${PS1_git_branch} ${PS1_dir} ${PS1_cmd_stat}${PS1_endl}${PS1_vi_state}${PS1_hist}${PS1_end}"
    zle reset-prompt
}

zle-line-init(){
    update-prompt
}
zle-keymap-select(){
    CUR_KEYMAP=$KEYMAP
    update-prompt
}
zle -N zle-keymap-select
zle -N zle-line-init
