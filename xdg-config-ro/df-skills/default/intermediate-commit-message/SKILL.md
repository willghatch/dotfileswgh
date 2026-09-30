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

Even when doing intermediate working commits, when starting a multi-commit project, especially on a new branch, include a first commit with a message that is about the overall purpose and intention of the feature.
This commit can be an empty commit with just a message.
The message should be descriptive about the work that will follow, and should include a short feature name in its title.
Keep using the short feature name in other commits in the series to make it clear from the titles that the commits go together.

Eg. here is an example log of commit headings for a feature branch, where the first commit is empty:

```
[subsystem] my-feature: frob the quux
[subsystem][tdd-red] my-feature: red test foo
[subsystem] my-feature: impl foo
[subsystem] my-feature: optimize foo
```

This helps the user when turning a branch into a PR, and it is helpful to delimit features for projects that keep all intermediate working commits without editing history.
