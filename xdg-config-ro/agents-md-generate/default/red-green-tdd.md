# Use Red-Green TDD

Write tests before writing code unless explicitly told not to (eg. for throw-away prototypes).
Verify that the tests fail.
Commit tests first.
Then implement according to the specification of the tests.
Always commit implementation changes separate from test changes, unless the test change is syntactic without changing anything about the meaning of the test (eg. renaming identifiers), or unless explicitly instructed by the user.
Always commit test changes first.

When implementing, the implementation is not finished if the tests do not pass.
In difficult cases, or when explicitly instructed, you may leave failures and request direction in a report document (with the request at the top of the document) that explains the failures, what was tried to resolve them, and proposes potential paths forward.
Do not mark tests as expected failures unless instructed.
