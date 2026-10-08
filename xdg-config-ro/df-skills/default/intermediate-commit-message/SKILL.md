---
name: intermediate-commit-message
description: Writing intermediate working commit messages.
---

# Intermediate commit messages

Messages for working commits made while a branch is still evolving.
Use the `pr-commit-message` skill for a final Pull Request commit instead.

Working commit messages should distinguish branch-level purpose from the details of this commit.
They help reviewers follow the branch's evolution and decide how to present the finished work.

Start with a one-line message focused on the commit's key purpose.
If useful, follow it with short paragraphs about the branch-level motivation and direction.
When adding commit-specific details, put the exact line `Change Details:` between those paragraphs and the details.
The one-line message is mandatory; `Change Details:` is mandatory only when details follow it.
Omit paragraphs that have nothing useful to say.

For a commit with several changes, end with a short list of what changed and why each change was needed.
The list is unnecessary when there is only one change.
Keep reasons brief but clear enough to support each code change.

Include `Assisted-by: AGENT_NAME:MODEL_VERSION` as a trailer.
Never include `Co-authored-by:`, `Claude-Session:` or a similar session trailer, or `Signed-off-by:`.

## Commit series

Even when doing intermediate working commits, when starting a commit series (IE. you will do more than one commit and they are related, whether or not on a topic branch), include a first commit with a message about the series' overall purpose and intention.
This commit can be an empty commit with just a message.
The message should describe the work that will follow and include a `[series:SERIES_NAME]` tag in its title.
Choose a short, descriptive `SERIES_NAME` for this particular set of changes; use a new name for each distinct commit series.
Repeat the series tag in the titles of the other commits so it is clear that they belong together.

Eg. here is an example log of commit headings for a series where the first commit is empty:

```
[subsystem][series:quux-frobbing] frob the quux
[subsystem][tdd-red][series:quux-frobbing] red test foo
[subsystem][series:quux-frobbing] impl foo
[subsystem][series:quux-frobbing] optimize foo
```

This helps the user when turning a branch into a PR, and it delimits commit series for projects that keep all intermediate working commits without editing history.
