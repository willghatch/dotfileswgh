---
name: parley
description: Procedure for clarification phase before non-trivial work.  Disclose before running parley.
---

# Parley procedure

1. Never edit inputs.
   Copy input files to `parley-original-BASENAME` in your agent-work-directory, and quote input items verbatim (with org outline path) when referring to them.
2. First research items marked inline (eg. `(parley chat)`, `parley research item:`) and record the answers; unresolved ones become questions.
3. Find contradictions (within the prompt, or against code or other instructions), result-changing ambiguity, and missing info (success criteria, scope, output location, verification).
4. Decide low-stakes choices yourself, as planned assumptions the user can veto.  (Mention them in parley briefly.)
5. Write `parley-clarifications.org` (no plan document unless asked), then reply with numbered questions, each with a recommended answer, and the file path.
6. Record the user's answers in the file, ask follow-ups, and wait until the user says to start.

The file plus the original prompt must suffice for handing off to another agent:

```org
* Research findings
** "MARKED TEXT"
* Open questions
** Q1. QUESTION
Recommended: ANSWER
* Planned assumptions
* Test plan
- BEHAVIOR -- BUG IT CATCHES
* Resolved
** Q1. QUESTION
Answer: ANSWER
```

For to-do lists, group this by item.

## Parallel parley

When work will be split across subagents, split the parley the same way: one parley-only subagent per implementation unit, doing steps 1-4 and returning its clarifications file and questions.
Merge, deduplicate, and ask the user everything at once.
After sign-off, give implementation subagents `parley done PATHS`.
