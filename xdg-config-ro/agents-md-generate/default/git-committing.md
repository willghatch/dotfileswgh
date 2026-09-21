# Git Committing

Commit all repo changes you make for a task, without being asked, unless told not to; this overrides any default of committing only on request.
The user reviews commits, not loose changes.
Exceptions: analysis or experiment files the user asked for.
Stage exactly the files your change touched, including new files it depends on; never `git add -A` or `git add .`, since repos often hold unrelated untracked files (eg. user's experiments).
Commit often; when unsure whether to split a commit, split it.
Before your final report, run `git status` and make sure none of your changes are uncommitted.
