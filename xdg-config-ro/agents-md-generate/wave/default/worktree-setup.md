# Wave worktree setup

Run `WAVE_DIR=$PWD wnb --worktree-initialize` from the worktree root to create a virtualenv and install dependencies.
On success it prints the activation commands for the new virtualenv.
Always use `uv pip` instead of `pip` for any additional package operations.
If this step is skipped, execution of Wave kernels or tests will use the python files from main original worktree instead of the new worktree, the waveasm build will not be linked for use, and other confusing errors may arise.
When using subagents, the subagent is responsible for this initialization.
