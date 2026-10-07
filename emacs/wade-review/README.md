# wade-review

Review a git branch as an org-mode checklist.

Vibe-coded review workflow -- these docs are generated, and may drift out of date.
But I decided to keep them, still.

- The `wade-review` CLI writes an org file with a TODO heading per changed file and per hunk.
- Each hunk is in a `diff` src block, with +/- backgrounds, fine highlights on changed text, and the file's language syntax highlighting.
- Unchanged code moved within or between files has a subtle yellow background and a brighter yellow `-` or `+` indicator.
- From a heading or a diff line, jump to that place in the file at the branch tip, in a dedicated review worktree.
- Files in the review worktree highlight lines and refined changed text since the comparison start, can show deleted code, and, if git-gutter is installed, enable `git-gutter-mode` against the same start (so its hunk navigation works too).

## Generating a review

Use the `wade-review` command, which wraps some elisp to be used on the command line.

By default, Wade reviews the current branch from its merge base with the repository's default branch.
Pass `--base REF` to select a different branch or commit for that merge-base calculation:

```sh
wade-review --base master --branch feature --output review.org
```

Pass `--from REF` to use an exact left endpoint without finding a merge base:

```sh
wade-review --from previously-reviewed --branch feature --output followup.org
```

`--base` and `--from` are mutually exclusive.

## The review file

The file's first line enables `org-mode` and `wade-review-mode`, once `wade-review-mode` is autoloaded (see Installation).

Write notes in sub-headings under a hunk or directly inside a diff block using `# ` at the start of the line.
Inline commentary has its own highlighting, does not affect language or fine-change highlighting, and does not advance source-line counts used for jumping.
Customize `wade-review-comment-prefix` before opening a review to use a different exact prefix.
Avoid other edits to diff text inside blocks, since jumping counts diff lines from the block's `@@` line.
Moved-line ranges are recorded as `WR_MOVED_OLD` and `WR_MOVED_NEW` properties on each hunk, using line numbers from the old and new files.
The `-` and `+` prefixes still show which side of the move is being viewed, with their own move-indicator faces.
Git's move detection is heuristic; Wade asks for substantial moved blocks, so isolated trivial matches and short genuine moves remain ordinary additions and deletions.

## Commands

`wade-review-command-map` holds the commands, keyed by single letters.
It is not bound to any key; bind it to a prefix of your choice.

| Key | Command | |
| --- | --- | --- |
| `j` | `wade-review-jump` | Visit the file and line for point (hunk start, or the exact line in a block). |
| `d` | `wade-review-toggle-deleted` | Show or hide deleted code in a worktree file. |
| `r` | `wade-review-refresh` | Recompute highlights in a worktree file. |
| `k` | `wade-review-close-worktree` | Remove the review worktree and kill its buffers. |
| `w` | `wade-review-open-worktree` | Open the worktree root in Dired. |

The first jump creates a detached worktree of the reviewed tip at `GIT_DIR/agent-files/wt/user/review-XXXXXX`, and records it in the review file as `#+WR_WORKTREE:`.
Since these are real files in a real checkout, LSP, xref, and project tools work as usual.
Jumps push the xref marker stack, so `xref-go-back` (or `pop-tag-mark`) returns to the review.
Deleted files open read-only, as of the comparison start.

In worktree files, `wade-review-highlight-mode` highlights lines against the comparison start.
Its faces inherit Emacs diff faces and set no foreground colors, so source syntax highlighting remains visible.
Changed portions within replacement lines also use Emacs's `diff-refine-added` face.
When deleted code is shown, its changed portions use `diff-refine-removed`.
Only moved-in and moved-out lines have Wade-specific background colors.
Moved-in lines use the same muted background as moved lines in the review.
When deleted code is shown, moved-out lines use that background too, while actual deletions keep the deletion style.
It compares the file on disk and refreshes on save.
For a file renamed on the branch, the highlights treat it as renamed only when it was visited by a jump.
Faces: `wade-review-added`, `wade-review-changed`, `wade-review-deleted`, `wade-review-moved-in`, `wade-review-moved-out`, `wade-review-moved-in-indicator`, `wade-review-moved-out-indicator`, `wade-review-comment`, and `wade-review-comment-indicator`.

## Tests

```sh
./run-tests.sh
```

The tests create temporary git repositories and use only `emacs -Q` and `git`.
