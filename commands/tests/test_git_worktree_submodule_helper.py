"""Tests for git-worktree-submodule-helper, run against real temporary repos.

Run with: python3 -m unittest discover -s commands/tests
"""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parent.parent / "git-worktree-submodule-helper"


class NestedSubmoduleRepos(unittest.TestCase):
    """Fixture: superproject A with submodule B at deps/b, which has submodule C at libs/c.

    Branch main records b1 (recording c1); branch feat records b2 (recording c2).
    The main checkout A is on main with all submodules checked out.
    """

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        # Resolve so paths compare equal to what git reports.
        self.root = Path(tmp.name).resolve()

        gitconfig = self.root / "gitconfig"
        gitconfig.write_text(
            "[user]\n\tname = Test\n\temail = test@example.com\n"
            "[init]\n\tdefaultBranch = main\n"
            "[protocol \"file\"]\n\tallow = always\n"
            "[worktree]\n\tuseRelativePaths = true\n"
            "[advice]\n\tdetachedHead = false\n"
        )
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        self.env["GIT_CONFIG_GLOBAL"] = str(gitconfig)
        self.env["GIT_CONFIG_NOSYSTEM"] = "1"

        src_c = self.make_repo("srcC")
        src_b = self.make_repo("srcB")
        self.git("submodule", "add", "-q", str(src_c), "libs/c", cwd=src_b)
        self.git("commit", "-qm", "b1", cwd=src_b)
        src_a = self.make_repo("srcA")
        self.git("submodule", "add", "-q", str(src_b), "deps/b", cwd=src_a)
        self.git("commit", "-qm", "a1", cwd=src_a)

        self.a = self.root / "A"
        self.git("clone", "-q", "--recurse-submodules", str(src_a), str(self.a), cwd=self.root)
        self.b1 = self.rev(self.a / "deps/b")
        self.c1 = self.rev(self.a / "deps/b/libs/c")

        self.git("checkout", "-qb", "feat", cwd=self.a)
        self.commit_file(self.a / "deps/b/libs/c", "c2")
        self.git("add", "libs/c", cwd=self.a / "deps/b")
        self.git("commit", "-qm", "b2", cwd=self.a / "deps/b")
        self.git("add", "deps/b", cwd=self.a)
        self.git("commit", "-qm", "a2", cwd=self.a)
        self.b2 = self.rev(self.a / "deps/b")
        self.c2 = self.rev(self.a / "deps/b/libs/c")
        self.git("checkout", "-q", "main", cwd=self.a)
        self.git("submodule", "update", "-q", "--recursive", cwd=self.a)

    # -- helpers ------------------------------------------------------------

    def run_git(self, *args, cwd):
        return subprocess.run(
            ["git", *args], cwd=cwd, env=self.env, text=True, capture_output=True
        )

    def git(self, *args, cwd):
        result = self.run_git(*args, cwd=cwd)
        if result.returncode != 0:
            self.fail(f"git {' '.join(args)} failed in {cwd}:\n{result.stderr}")
        return result.stdout.strip()

    def helper(self, *args, cwd, env=None):
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            cwd=cwd,
            env=env or self.env,
            text=True,
            capture_output=True,
        )

    def helper_ok(self, *args, cwd):
        result = self.helper(*args, cwd=cwd)
        if result.returncode != 0:
            self.fail(f"helper {' '.join(args)} failed:\n{result.stdout}{result.stderr}")
        return result

    def make_repo(self, name):
        path = self.root / name
        self.git("init", "-q", str(path), cwd=self.root)
        self.commit_file(path, name)
        return path

    def commit_file(self, repo, content):
        (repo / "f").write_text(content + "\n")
        self.git("add", "f", cwd=repo)
        self.git("commit", "-qm", content, cwd=repo)

    def rev(self, repo):
        return self.git("rev-parse", "HEAD", cwd=repo)

    def common_dir(self, repo):
        return Path(self.git("rev-parse", "--path-format=absolute", "--git-common-dir", cwd=repo)).resolve()

    def assert_main_checkout_healthy(self):
        """The main checkout's submodules resolve to themselves and are unchanged."""
        status = self.run_git("status", "--porcelain", cwd=self.a)
        self.assertEqual(status.returncode, 0, status.stderr)
        self.assertEqual(status.stdout, "")
        for sub in ("deps/b", "deps/b/libs/c"):
            top = self.git("rev-parse", "--show-toplevel", cwd=self.a / sub)
            self.assertEqual(Path(top).resolve(), self.a / sub)
        self.assertEqual(self.rev(self.a / "deps/b"), self.b1)
        self.assertEqual(self.rev(self.a / "deps/b/libs/c"), self.c1)

    def assert_shares_storage_with_main(self, wt):
        for sub in ("deps/b", "deps/b/libs/c"):
            self.assertEqual(self.common_dir(wt / sub), self.common_dir(self.a / sub))


class AddTest(NestedSubmoduleRepos):
    def test_nested_submodules_use_commits_recorded_by_the_new_worktree(self):
        # Regression: commits were read from the main checkout's tree, so a
        # worktree of feat got main's b1/c1 and started out dirty.
        wt = self.root / "wt"
        self.helper_ok("add-worktree", str(wt), "-b", "feat", cwd=self.a)

        self.assertEqual(self.rev(wt / "deps/b"), self.b2)
        self.assertEqual(self.rev(wt / "deps/b/libs/c"), self.c2)
        self.assertEqual(self.git("status", "--porcelain", cwd=wt), "")
        self.assert_shares_storage_with_main(wt)
        self.assert_main_checkout_healthy()

    def test_submodules_not_checked_out_in_main_come_from_shared_storage(self):
        # On branch extra, A gains submodule tools/d (cloned once, so its git
        # data exists in A/.git/modules) and tools/e (never cloned: no git data).
        src_d = self.make_repo("srcD")
        self.git("checkout", "-qb", "extra", cwd=self.a)
        self.git("submodule", "add", "-q", str(src_d), "tools/d", cwd=self.a)
        self.git("config", "--file", ".gitmodules", "submodule.e.path", "tools/e", cwd=self.a)
        self.git("config", "--file", ".gitmodules", "submodule.e.url", str(src_d), cwd=self.a)
        self.git(
            "update-index", "--add", "--cacheinfo", f"160000,{self.rev(src_d)},tools/e", cwd=self.a
        )
        self.git("add", ".gitmodules", cwd=self.a)
        self.git("commit", "-qm", "extra", cwd=self.a)
        self.git("checkout", "-q", "--recurse-submodules", "main", cwd=self.a)
        self.assertFalse((self.a / "tools/d/.git").exists())

        wt = self.root / "wt"
        result = self.helper_ok("add-worktree", str(wt), "-b", "extra", cwd=self.a)

        self.assertEqual(self.rev(wt / "tools/d"), self.rev(src_d))
        self.assertEqual(self.common_dir(wt / "tools/d"), self.a / ".git/modules/tools/d")
        self.assertIn("tools/e", result.stderr)
        self.assertFalse((wt / "tools/e/.git").exists())
        self.assert_shares_storage_with_main(wt)


class SubmoduleUpdateTest(NestedSubmoduleRepos):
    def test_follows_branch_switch_without_breaking_main_checkout(self):
        # `git submodule update` in a linked worktree rewrites the shared
        # core.worktree and breaks the main checkout; submodule-update must not.
        wt = self.root / "wt"
        self.helper_ok("add-worktree", str(wt), cwd=self.a)
        self.git("checkout", "-q", "feat", cwd=wt)

        self.helper_ok("submodule-update", "--recursive", cwd=wt)

        self.assertEqual(self.rev(wt / "deps/b"), self.b2)
        self.assertEqual(self.rev(wt / "deps/b/libs/c"), self.c2)
        self.assertEqual(self.git("status", "--porcelain", cwd=wt), "")
        self.assert_shares_storage_with_main(wt)
        self.assert_main_checkout_healthy()


class RemoveTest(NestedSubmoduleRepos):
    def test_removes_nested_worktrees_and_keeps_locked_submodule_worktrees(self):
        # A locked worktree whose checkout is currently missing (eg. on an
        # unmounted drive) must survive, as it does with `git worktree prune`.
        locked = self.root / "locked-b"
        self.git("worktree", "add", "-q", "--detach", str(locked), cwd=self.a / "deps/b")
        self.git("worktree", "lock", str(locked), cwd=self.a / "deps/b")
        shutil.rmtree(locked)

        wt = self.root / "wt"
        self.helper_ok("add-worktree", str(wt), cwd=self.a)
        self.helper_ok("remove-worktree", str(wt), cwd=self.a)

        self.assertFalse(wt.exists())
        b_worktrees = self.git("worktree", "list", "--porcelain", cwd=self.a / "deps/b")
        c_worktrees = self.git("worktree", "list", "--porcelain", cwd=self.a / "deps/b/libs/c")
        self.assertIn(str(locked), b_worktrees)
        self.assertNotIn(str(wt), b_worktrees)
        self.assertNotIn(str(wt), c_worktrees)
        self.assertNotIn(str(wt), self.git("worktree", "list", "--porcelain", cwd=self.a))
        self.assert_main_checkout_healthy()

    def test_repairs_main_checkout_after_forbidden_git_submodule_update(self):
        wt = self.root / "wt"
        self.helper_ok("add-worktree", str(wt), cwd=self.a)
        self.git("submodule", "update", "-q", "--recursive", cwd=wt)
        self.assertNotEqual(self.run_git("status", cwd=self.a).returncode, 0)

        self.helper_ok("remove-worktree", str(wt), cwd=self.a)

        self.assert_main_checkout_healthy()


class GitVersionTest(NestedSubmoduleRepos):
    def test_refuses_git_older_than_minimum(self):
        real_git = shutil.which("git", path=self.env.get("PATH"))
        fake_bin = self.root / "fake-bin"
        fake_bin.mkdir()
        fake_git = fake_bin / "git"
        fake_git.write_text(
            "#!/bin/sh\n"
            'case "$1" in --version|version) echo "git version 2.47.1"; exit 0;; esac\n'
            f'exec "{real_git}" "$@"\n'
        )
        fake_git.chmod(0o755)
        env = dict(self.env, PATH=f"{fake_bin}{os.pathsep}{self.env.get('PATH', '')}")

        wt = self.root / "wt"
        result = self.helper("add-worktree", str(wt), cwd=self.a, env=env)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("2.47.1", result.stderr)
        self.assertIn("2.48.0", result.stderr)
        self.assertFalse(wt.exists())


if __name__ == "__main__":
    unittest.main()
