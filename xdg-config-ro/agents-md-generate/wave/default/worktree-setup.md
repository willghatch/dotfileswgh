# Wave worktree setup

Before initializing a new worktree, clean up stale environment from other worktrees:
- If a virtualenv is active, run `deactivate` first.
- When working with a local build of iree-compiler, `PYTHONPATH` may include pointers into iree-compiler that you need to keep.  But otherwise, it is typically set to `WAVE_DIR` and should be unset if it points to a different WAVE_DIR (eg. a different worktree).  Usually this just means unset it, but check it first.
- Unset stale Wave environment variables: `unset WAVE_LLVM_DIR WAVE_WATER_DIR WAVE_WAVEASM_DIR`.

Run `WAVE_DIR=$PWD $WAVE_DIR/build_tools/wave-dev-setup.sh --worktree-initialize` from the worktree root to create a virtualenv and install dependencies.
On success it prints the activation commands for the new virtualenv.
Always use `uv pip` instead of `pip` for any additional package operations.
If this step is skipped, execution of Wave kernels or tests will use the python files from main original worktree instead of the new worktree, the waveasm build will not be linked for use, and other confusing errors may arise.
When using subagents, the subagent is responsible for this initialization.

This worktree setup install provides necessary commands.
If needed commands are missing or tests fail to run, it likely means you skipped this step or ran it with stale environment variables.
Do this setup immediately after creating a worktree.
This may be skipped only for a read-only investigation.
