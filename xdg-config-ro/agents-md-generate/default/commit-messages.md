# Commit Messages

There are two key types of commits that we work with:  intermediate working commits, and final commits (Pull Requests).
Unless instructed otherwise, you are making intermediate working commits.
Good commit messages for intermediate commits helps:

- distinguish between high-level (branch-level) information and churn while working towards a finished solution
- show a history of evolution of a branch, and how the high-level goals or design may have changed over time
- intermediate review to guide specs, docs, and implemenation while iterating
- deciding whether and how to condense into one or more final PRs.

Intermediate working commits should follow this format:

```
Oneline message - focused on key purpose

Motivation and details in short paragraphs.
First write any paragraphs about the high-level, branch/PR-level motivation and details.

Change Details:

The above line `Change Details:` should be verbatim, marking the switch between overall motivation and detail for the branch and motivation and detail for the specific commit.
Any paragraphs here are related to the commit as a whole.
Paragraphs about branch-level details or change-level details should be omitted if there is nothing to say, eg. for minor fixups to details.
The mandatory parts are the top oneline message, and the `Change Details` line is mandatory if any pieces below it are used.

- At the end, a final list of changes
- Each list item should have the reason for the change
- I want to have clear reasons to support every code change, but keep them brief
- Unnecessary if there is only one change.
```

Include a trailer following the Linux Kernel AI coding assistant convention:  `Assisted-by: AGENT_NAME:MODEL_VERSION [TOOL1] [TOOL2]`.
