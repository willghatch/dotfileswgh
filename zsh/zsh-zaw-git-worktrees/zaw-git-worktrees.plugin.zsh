if [[ -n $(declare -f -F zaw-register-src) ]]; then
    here=${${0:A}:h}
    for fn in $(ls $here/sources/); do
        source $here/sources/$fn
    done
else
    echo "zaw-git-worktrees plugin not loaded since zaw is not loaded."
    echo "Please load zaw (https://github.com/zsh-users/zaw) first."
fi
