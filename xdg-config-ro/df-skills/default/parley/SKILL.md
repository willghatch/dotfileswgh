---
name: parley
description: Procedure for clarification phase before non-trivial work.
---

# Parley

## Procedure

1. First research items marked inline (eg. `(parley q)`, `parley research:`).
   Record answers and related questions.
2. Find contradictions (within the prompt, or against code or other instructions), result-changing ambiguity, and missing info (success criteria, scope, output location, verification).
3. Decide low-stakes choices yourself, as planned assumptions the user can veto.
4. Write `parley-clarifications.org`.  Be concise.  Give the file path in chat.  It should include or reference the original prompt.
5. The user edits the file.
6. Iterate.  Append to the file only, never edit previous questions or answers.  Ask follow-up questions, move forward in a decision tree, etc.  Get explicit user sign-off before implementation.

## Example clarification file format:

```org
* Original Prompt
[Prompt from interactive chat, or file path]
* Parley Round 1
** Research findings
** Questions
*** Q1. QUESTION
[optional context]
**** Recommended
[answer]
**** Option B
[answer]
**** User:
[answer]
*** Q2. QUESTION
**** Recommended
[answer]
User agreed.
**** Alternate
[answer]
** Planned assumptions
** Test plan
- BEHAVIOR -- BUG IT CATCHES
* Parley Round 2
...
```

Among answer options, the user can mark acceptance by writing `(user agreed)`, `user: answer`, `user clarification: ...`, etc.
Don't write a blank `User:` heading, the user will add it if necessary.

## Work

Once work starts, don't stop to ask unless a question is truly important; guess and continue.
Stop for real blockers (data loss, unauthorized irreversible or outward-facing actions, impossible task).
Record each guess when made in `parley-assumptions.org` (agent-work-directory): one heading each with question, choice and why, and affected files/commits.
Your final report links every assumptions file (yours and subagents') or says there are none.
For questions and reports, state assumptions in the answer instead.

Subagent prompts must say `parley done PATHS` or `skip parley`; subagents never wait for sign-off.

## Pedantic Control Phrases

- `parley in chat`, `parley just chat` - skip writing files, just do QA in chat.
- `parley chat` - before writing or stepping in parley file procedure, just chat to clarify with the user.  IE don't skip the file workflow like `parley in chat`, but have a few rounds of chat-only discussion about the flagged topic (IE usually this is inline as `(parley chat)` to mark a part of the spec where the user lacks some understanding.  `parley resume` to mark being done with interactive chat.
