# Subagents must use worktrees

When subagents may need to edit files, or explore files in an active git checkout, they must use temporary worktrees.
The agent that spawns the subagent must provide them with the worktree path.
The spawning superagent decides whether to create the worktree or delegate worktree creation to the subagent, but the agent that creates the worktree must also remove it.
