# Use Red-Green TDD

Red-green TDD is the default for behavior changes, unless told otherwise (eg. throw-away prototypes).
Bug fixes always start with a test that reproduces the bug.

Before writing tests, write `test-plan.org` in your agent-work-directory: one line per test, naming the behavior and the bug it would catch.
Review it against the Test Writing Guidelines and cut tests that fail them.

Verify that new tests fail for the right reason (wrong or missing behavior, not eg. an import error).
Commit tests first, then the implementation, separately, unless the test change is purely syntactic (eg. renames) or instructed otherwise.
Add a `[tdd-red]` prefix to the commit message for failing test commits.

Not every change needs a new test: refactors are covered by existing tests, and docs, comments, formatting, and config or data values usually need none.
When skipping a test for a behavior change, give the reason in one line in the commit message.

The implementation is not finished until the tests pass.
In difficult cases, or when instructed, you may leave failures and request direction in a report (request at the top) explaining the failures, what was tried, and possible paths forward.
Do not mark tests as expected failures unless instructed.
