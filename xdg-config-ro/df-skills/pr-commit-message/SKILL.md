---
name: pr-commit-message
description: How to write commit messages for PRs (pull requests), rather than intermediate working commits
---

# PR Commit Messages

Typically I make PRs after a series of working commits.
Working commits tend to have a mix of detail for the branch/feature/PR as a whole, and detail about individual changes.
The PR commit message needs to synthesize all of that into a single cohesive message.

The PR oneline commit header needs to BRIEFLY address, in order of priority:

1. The high-level motivating purpose of the PR
2. The key technical change of the PR

If there is not room for both, prefer the high-level motivating purpose, stating the key technical change in one line seperate from any paragraphs after it.
Sometimes the key technical change is the high-level motivating purpose.
If there is more than one key technical change, it is probably best left for the longer description.

Example:

```
Reduce register pressure via loop invariant code motion improvement
```

After the header line (and optional one-line follow-up to it), the PR commit message needs to address, in order of importance:

1. The high-level purpose or motivation (if not fully captured by the header line).
2. The key technical change(s) of the PR.
3. A list motivating each smaller change.

Each of these should be separate: the high-level motivation and key technical changes should be in separate paragraphs unless the technical change is also the motivation.

The list of small changes is helpful for code review and for code archaeology.
IE for every changed function, it is common for reviewers to need rationales for changes on a function-by-function basis.
Sometimes changes seem unnecessary or strange.
These should all have a rationale for why they change.
This change rationale does not belong in source code comments, but it does belong in a commit message.
This list of changes should explicitly be in a markdown list.

Put the most important things first -- this is the rationale behind the overall structure outlined, but it should also be reflected in list ordering and paragraph structure.

Use structure in the commit message.
If the high-level discussion or key technical changes require multiple paragraphs, use section headers to make it clear where the division is.
If multiple paragraphs are needed for something, consider using bulleted lists instead (always use section headers when including bulleted lists).
Even nested lists may be appropriate for large or complicated PRs.

PR commit messages are read by a mix of audiences.
They are read by the original author later to remember rationale and details.
They are read by peer reviewers to determine whether to merge a PR or make changes.
They are read by experts with deep familiarity.
They are read by outside observers with little familiarity.
In addition to having the most important information first, try to use both expert detailed language and more high-level language.
Assume that all readers of sections about technical changes or details have at least a general background in software development as a floor.
(Eg. you can always use well-known jargon for that.)
When discussing details that require deep domain-specific knowledge (eg. very specific tech stack details, library-specific details, specific compiler passes or optimizations, hardware architecture or assembly details, specific variants of data structures or algorithms, networking details, etc), define all acronyms used (eg. `... changes to the LICM (Loop Invariant Code Motion) pass`), and provide extra detail.
It is common that expert reviewers on the same team do not know or remember all of the acronyms, key details, or even overall purpose of many of these things.
