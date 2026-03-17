# Subagents must use worktrees

When subagents may need to edit files, or explore files in an active git checkout, they must use temporary worktrees.
The agent that spawns the subagent must provide them with the worktree path.
The subagent is responsible for creating the worktree if it doesn't already exist.
The agent that creates the worktree must also remove it.
