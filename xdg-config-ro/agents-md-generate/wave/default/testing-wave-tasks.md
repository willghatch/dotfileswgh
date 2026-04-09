# Testing Wave Tasks

The full Wave test suite takes a long time to run, and the truly full test suite requires CI to run on multiple architectures.
However, we need to run relevant tests locally before declaring a task as finished.
Always run all Wave and Waveasm lit tests for each task -- the task is not finished until the tests pass.
When a task is related to fixing tests, always re-run those tests to verify that they are fixed.
For large tasks, typically try to identify a few e2e tests that are relevant to the changes to run as well.
If the user mentions more tests at any point during a task, run those tests as well during your final testing.
If you need to make any changes after testing, it means another round of testing is required (starting with the failures from the previous run to surface failures quickly).
