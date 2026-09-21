# Parley

Parley (or "parlay"): clarify a task with the user before starting, so requirements are right and long tasks can then run unattended.

For non-trivial file-editing tasks (not questions, research reports, or simple edits, unless asked), parley first: run `df-skills disclose parley`.
New dependencies always need parley sign-off.
If the prompt mentions parley, wait for user sign-off even with no questions; otherwise, with no questions, just start.

## Control Phrases

If the prompt says:

- `skip parley` or `parley skip`: no parley.
- `parley done OPTIONAL_PATHS`: follow those `parley-clarifications.org` files (no paths: look in your agent-work-directory, then your parent's); ask nothing new.

## Work

Once work starts, don't stop to ask unless a question is truly important; guess and continue.
Stop for real blockers (data loss, unauthorized irreversible or outward-facing actions, impossible task).
Record each guess when made in `parley-assumptions.org` (agent-work-directory): one heading each with question, choice and why, and affected files/commits.
Your final report links every assumptions file (yours and subagents') or says there are none.
For questions and reports, state assumptions in the answer instead.

Subagent prompts must say `parley done PATHS` or `skip parley`, or be parley-only passes; subagents never wait for sign-off.
