---
name: df-skill-authoring
description: Tips for writing new skill files, or editing skill files.  Also for editing AGENTS.md files.
---

# df-skill authoring

Follow general style guidelines (eg. prefer one-sentence-per-line).

The `df-skills` command supports multiple skill formats.
But prefer the default typical skill format (SKILL.md) unless there is a good reason to do something else.
(The good reason is mainly if there is real benefit to the skill text being dynamic, in which case using a programatic skill could be good, but get sign-off from the user about that.)

The `description` field for the skill is typically placed in every agent context.
It needs to (1) be as succinct as possible to save on tokens, but (2) be clear enough that agents actually disclose it when needed (and clear enough that agents don't defensively disclose it when they don't really need it).
The term `description` is a bit of a misnomer -- it should not be focused on what a skill does, but rather about how should an agent know when it should be disclosed.
If more metadata or commentary would be helpful, it can go in other frontmatter tags, which are not shown when disclosing and thus don't bloat context.

Skills should not be full of general knowledge or repeat things that will already be in the agent's context.
They should focus on things like:

- context-specific knowledge
- things that are abnormal, non-default, not assumed -- info that loads a context to overcome otherwise default behavior
- procedural information
- constraints

The key idea of skills is progressive disclosure.
IE skills are for re-usable context to steer agent behavior, but that is not so universal as to always put it in an AGENTS.md file.
Skills can themselves reference sub-skills.
Eg. skill `foo` can say "Under these more specific circumstances, `df-skills disclose foo/sub-skill-1`.
That way a skill can lean harder into progressive disclosure if it would otherwise be long and have subsections that are not always relevant.
Don't go crazy with this, though -- most skill files are only rarely disclosed, and so the context savings of nested skills are not nearly as important as brevity in AGENTS.md files and skill descriptions.

This all applies to authoring agents-md-generate snippets or AGENTS.md files.
They are basically the default, always-on skills.
AGENTS.md snippets should be as brief as possible.
A common pattern is to have paired agents-md-generate snippets with skill files.
The agents-md-generate snippet includes universal policy about when to do something, but points to a df-skills skill for the procedure when it happens, or for more fine-grained detail about deciding in grey areas, or such.

Another pattern that I use is "command words" or phrases.
Eg. the user can use some cryptic or made-up word or phrase to signal non-default behavior.
It can trigger skill loading, where the skill description just tells what the command word is, and the skill body can say what to do when it is used.
