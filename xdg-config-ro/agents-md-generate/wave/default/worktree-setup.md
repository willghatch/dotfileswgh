# Wave worktree setup

Before initializing a new worktree, clean up stale environment from other worktrees:
- If a virtualenv is active, run `deactivate` first.
- Unset stale Wave environment variables: `unset WAVE_DIR WAVE_LLVM_DIR WAVE_WATER_DIR WAVE_WAVEASM_DIR PYTHONPATH WAVE_CACHE_ON`.

Run `WAVE_DIR=$PWD $WAVE_DIR/build_tools/wave-dev-setup.sh --worktree-initialize` from the worktree root to create a virtualenv and install dependencies.
On success it prints the activation commands for the new virtualenv.
Always use `uv pip` instead of `pip` for any additional package operations.
If this step is skipped, execution of Wave kernels or tests will use the python files from main original worktree instead of the new worktree, the waveasm build will not be linked for use, and other confusing errors may arise.
When using subagents, the subagent is responsible for this initialization.
