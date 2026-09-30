## Agent work directories

Write any scratch files, plans, notes, and intermediate results in your agent-work-directory.
Link relevant work files in your chat reply so the user can find them.
If not given an agent-work-dir, run `agent-work-dir-base --resolve` from your starting directory, then append `TIMESTAMP_TOPIC/` to make your agent-work-directory.
Use an explicit agent-work-directory path if one is given.

- **TIMESTAMP**: output of `date "+%Y-%m-%dT%H-%M-%S"`.
- **TOPIC**: 1–3 hyphenated words from the branch name or task.

Files that are part of the actual implementation (code, docs, tests) go in the normal repo structure.
Other documentation or tests are temporary working files.

When launching subagents, pass them a child dir of your agent-work-directory as their explicit agent-work-directory path.
You may share files between agents via these paths.
Do not read or write another agent's agent-work-directory unless requested by the user or manager agent.
