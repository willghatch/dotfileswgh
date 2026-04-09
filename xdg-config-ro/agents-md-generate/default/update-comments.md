# Updating Docs and Comments

When updating code, often there are relevant comments nearby but not immediately next to the code edited.
Check each surrounding scope (eg. surrounding blocks, functions, file level) for comments, docstrings/doc-comments, or documentation that should be updated.

Only update comments meaningfully -- don't churn comment phrasing unless there is a substantive change that needs to be made.
Don't update documentation to reflect internal implementation details (unless the documentation already discusses implementation details that have been invalidated) -- that belongs in source code comments.

Remember:  The official documentation should be the clear and primary source of truth (specification), followed by docstrings of public APIs, followed by tests, followed by docstrings of private APIs, followed by the source code comments, followed by code.
They should all be in agreement, though with different levels of detail.
Official documentation should be the core source of truth, with tests acting as a more detailed specification.
Docstrings on internal/private APIs are not public or official documentation.
In practice, these levels have often drifted out of sync in the past.
Avoid further drift.
If you notice other disagreements that are not part of your task, you may mention them in a report, but do not change them.
Stay focused on your task.

Keep docstrings focused on high-level semantics.
Leave implementation details for comments.
Comments with low-level implementation details may be immediately next to docstrings or doc comments, but should not be included in them.

Do not add or edit comments to explain a change where the comment is not valuable as in-source documentation permanently.
Bad example: `(frob-item bar) ;; Change from foo to support new feature`.
IE comments should be a good explanation for the code, not for changes to code.
Good example: `(frob-item foo #:weird-option #t) ;; Need to set weird option for correct behavior with quux library`.
Comments explaining the reason for a change should go in the commit message, not in the source code.
