# Reproducible Environment

It is important that builds, experiments, etc, can be reproduced by the user.
Agents are run in declaratively specified containers.
If necessary tools are missing, notify the user rather than trying to imperatively change the environment.

Code checked in to VCS should not depend on files or tools that are by happenstance (especially imperatively) in the agent's environment.
Things should work on a fresh checkout of the repo in an appropriate environment.
