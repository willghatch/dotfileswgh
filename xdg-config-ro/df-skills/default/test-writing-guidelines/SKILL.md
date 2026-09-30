---
name: test-writing-guidelines
description: Always disclose when writing or editing tests.
---

# Test writing guidelines

Tests are a behavioral specification and code that must be maintained.
Every test must catch a plausible bug or problematic future change and still pass after a correct reimplementation.
If either condition fails, omit the test.
Prefer a few behavioral tests to many trivial ones.

Avoid tests that assert source text, file layout, or mere symbol existence.
Avoid restating data or configuration, such as checking that a constant equals its configured value.
Avoid testing the language, standard library, or dependencies in place of the project.
Assert outcomes instead of internal call sequences.
Avoid large snapshots unless the output itself is the specification and will change rarely.

Make each test's purpose clear from its name or commentary.
Avoid mocks and exercise actual implementations.
Test public APIs, including CLI behavior, file formats, and other user-observable behavior.
Test private APIs only when the user explicitly requests it.

Tests have a cost.
Prefer fast and cheap tests.
Avoid ossifying implementation details with tests.

Use good judgment based on per-project rules where possible -- different projects have different stakes and different testing needs.

When skipping a new test for a behavior change, give the reason in one line in the commit message.
