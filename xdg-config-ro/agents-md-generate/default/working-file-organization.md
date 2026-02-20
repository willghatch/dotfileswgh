# File Organization

Write scratch files, notes, and intermediate artifacts to `REPO/.git/agent-files/work/TIMESTAMP_TOPIC_AGENTID/` (your **agent-work-directory**), not the repo root or `/tmp/`.

- **TIMESTAMP**: output of `date "+%Y-%m-%dT%H-%M"`.
- **TOPIC**: 1–3 hyphenated words from the branch name or task.
- **AGENTID**: use an ID from the prompt, or generate one: `head -c 10 /dev/random | md5sum | head -c 6`.
- **REPO**: the root of the git repo you started in. If `.git` is a file (submodule/worktree), read it to find the host repo's `.git` directory—do not place the agent-work-directory under `.git/modules/` or `.git/worktrees/`.
- **Not in a git repo**: use `agent-files/work/TIMESTAMP_TOPIC_AGENTID/` relative to your starting directory.
- **Explicit agent-work-directory path given**: use that instead.

Example: `$HOME/projects/widgets/.git/agent-files/work/2026-01-01T10-10_display_bb946a/` might contain `investigation.md`, `plan.org`, `results.md`.

Files that are part of the actual implementation (code, docs, tests) go in the normal repo structure, not the agent-work-directory.
Documentation or tests that do not fit the repo structure are temporary working files, not implementation.

When launching subagents, pass them an explicit agent-work-directory path (nested under yours, or with a meaningful AGENTID).
You may share files between agents via these paths.

Do not read or write another agent's agent-work-directory unless requested.
