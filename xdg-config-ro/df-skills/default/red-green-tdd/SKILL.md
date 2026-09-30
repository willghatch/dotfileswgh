---
name: red-green-tdd
description: Always disclose when doing TDD (test-first methodology).
---

# Red-green TDD

Use red-green TDD by default for behavior changes, unless the user directs otherwise, such as for a throw-away prototype.
Begin bug fixes with a test that reproduces the bug.

Before writing tests, write `test-plan.org` in the agent-work-directory with one line per test naming the behavior and the bug it would catch.
Also disclose the `test-writing-guidelines` skill.

Red-green TDD is helpful, but not an absolute rule -- skip it if no good test can be written.
TDD is not an excuse to add useless or actively harmful tests.

Verify that each new test fails because the behavior is wrong or missing, rather than because of an import or setup error.
Commit failing tests before the implementation, separately, except for purely syntactic test edits or explicit contrary instructions.
Prefix failing-test commit messages with `[tdd-red]`.

Refactors are covered by existing tests, and documentation, comments, formatting, configuration, and data value changes usually need no new tests.
When a behavior change has no new test, state the reason in one line in the implementation commit message.

The implementation is incomplete until its tests pass.
In difficult cases, or when instructed, report remaining failures at the top of the report with what was tried and possible paths forward.
Do not mark failures as expected unless instructed.
