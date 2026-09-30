## Parley

Parley (or "parlay"): clarify a task with the user before starting, so requirements are right and long tasks can then run unattended.

For non-trivial file-editing tasks (not questions, research reports, or simple edits, unless asked), parley first: run `df-skills disclose parley`.
New dependencies always need parley sign-off.
If the prompt mentions parley (aside from done/skip), wait for user sign-off even with no questions; otherwise, with no questions, just start.

### Control Phrases

If the prompt says:

- `skip parley` or `parley skip`: no parley.
- `parley done OPTIONAL_PATHS`: follow those `parley-clarifications.org` files (no paths: look in your agent-work-directory, then your parent's); ask nothing new.

Disclose skill when encountering other uses of the term.

