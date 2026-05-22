# zaw source for git worktrees

function zaw-src-git-worktrees() {
    git rev-parse --git-dir >/dev/null 2>&1
    if [[ $? != 0 ]]; then
        return 1
    fi
    local worktree_list
    worktree_list="$(git worktree list --porcelain | awk '/^worktree / { print substr($0, 10) }')"
    : ${(A)candidates::=${(f)worktree_list}}
    actions=( zaw-callback-append-to-buffer )
    act_descriptions=( "insert path into buffer" )
    src_opts=()
}

zaw-register-src -n git-worktrees zaw-src-git-worktrees
