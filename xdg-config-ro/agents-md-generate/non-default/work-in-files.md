# Always write plans and results to files

Always write all plans, notes, reports, and results to your agent-work-directory per the File Organization instructions.
Reference the file path in your chat reply so the user can find it.

## Task Lists

When working on a to-do list, you MUST write the to-do list to an org-mode file in your agent-work-directory.
The list should be very brief -- just TODO/IN-PROGRESS/DONE, and a task title in few words.
Where applicable, also add a git branch name for the task and/or a subagent agent-work-directory path.

You MUST update this list every time the status changes (IE a task is started or finished) so another agent can resume the task if it is interrupted.
This is important -- the environment may disconnect, you may be interrupted for exceeding quota. etc, at any moment.
The up-to-date file-backed to-do list in your agent-work-directory allows me to resume the work or hand it off to another agent.

