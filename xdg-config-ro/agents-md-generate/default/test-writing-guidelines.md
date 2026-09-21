# Test Writing Guidelines

Tests are a behavioral specification and code that must be maintained.
Every test must answer two questions: what plausible bug or problematic future change would it catch, and would it still pass after a correct reimplementation?
If either answer is bad, don't write it.
Prefer a few behavioral tests to many trivial ones.

Anti-patterns:
- Asserting on source text, file layout, or that a symbol exists.
- Restating data or config (asserting a constant, table, or resource equals what it was set to).
- Testing the language, standard library, or dependencies rather than this project.
- Asserting internal call sequences instead of outcomes.
- Large snapshots nobody will review, unless the output is itself the spec (and will be low-churn).

Make each test's purpose clear from its name and/or commentary.

Avoid mocks.
Test actual implementations.

Test public APIs, which include CLI behavior, file formats, and other user-observable behavior.
Only test private APIs with explicit instructions from the user.
