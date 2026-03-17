# Wave worktree setup

Run `WAVE_DIR=$PWD wnb --worktree-initialize` from the worktree root to create a virtualenv and install dependencies.
On success it prints the activation commands for the new virtualenv.
Always use `uv pip` instead of `pip` for any additional package operations.
