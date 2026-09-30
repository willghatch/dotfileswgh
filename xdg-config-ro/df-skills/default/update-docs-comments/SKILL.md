---
name: update-docs-comments
description: Disclose when adding or editing docs or comments.
---

# Updating docs and comments

When changing code, inspect comments, docstrings, and documentation in the surrounding scopes, including file-level and function-level material.
Update material made inaccurate by the change, without churning phrasing or making unrelated edits.
If you notice unrelated disagreements, mention them in the report instead of changing them.

Official documentation is the primary specification, followed by public API docstrings, then tests, private API docstrings, and source comments and code.
Keep these sources in agreement at their appropriate level of detail.
Documentation should describe behavior rather than new internal implementation details, unless existing documentation about those details has become inaccurate.
Keep docstrings focused on high-level semantics and put low-level implementation reasons in source comments.

Add or edit a comment only when it will remain useful as in-source documentation.
Do not write comments that describe the edit itself, such as “Changed from foo to support bar”.
Put the reason for the change in the commit message instead.
Comments and docs should make sense years later in a different context where readers don't know the history of the repo; they should never need context about the current task.
