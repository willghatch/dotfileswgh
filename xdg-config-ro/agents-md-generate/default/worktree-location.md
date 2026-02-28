# Git worktree location

When creating git worktrees, always place them in `.git/agent-files/wt/WORKTREE_NAME` in the main repo that the work is done in.
If you need to make a worktree for a submodule, use `.git/agent-files/wt/` in the (recursive) outer git repo, not `.git/modules/.../agent-files/wt/`.
