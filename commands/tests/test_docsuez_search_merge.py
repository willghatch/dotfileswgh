"""Behavioral tests for docsuez Dockerfile search-merge composition.

Run with: python3 -m unittest discover -s commands/tests
"""

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


DOCSUEZ = Path(__file__).resolve().parent.parent / "docsuez"


class SearchMergeTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name).resolve()
        self.first = self.root / "first"
        self.second = self.root / "second"
        self.first.mkdir()
        self.second.mkdir()
        self.env = dict(os.environ)
        self.env.update({
            "DOCSUEZ_PATH": os.pathsep.join((str(self.first), str(self.second))),
            "DOCSUEZ_NO_USER_NAME_PREPEND": "1",
            "XDG_CACHE_HOME": str(self.root / "cache"),
        })

    def write(self, path, content):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        return path

    def write_json(self, path, value):
        return self.write(path, json.dumps(value))

    def make_build(self, dockerfile, name="env"):
        return self.write_json(
            self.first / name / "build-docsuez.json",
            {"dockerfile": dockerfile, "image-name-postfix": f"{name}-image:latest"},
        )

    def docsuez(self, *args, env=None):
        return subprocess.run(
            [sys.executable, str(DOCSUEZ), *args],
            cwd=self.root,
            env=env or self.env,
            text=True,
            capture_output=True,
        )

    def dry_show(self, name="env"):
        return self.docsuez("build", name, "--dry-show")

    def assert_success(self, result):
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def assert_in_order(self, text, *parts):
        positions = [text.index(part) for part in parts]
        self.assertEqual(positions, sorted(positions), text)

    def test_merges_file_and_directory_contributions_by_position_and_origin_order(self):
        self.make_build([
            {"text": "START"},
            {"search-merge": "extensions/hook"},
            {"search": "ordinary"},
            {"text": "END"},
        ])
        first_hook = self.first / "extensions/hook"
        self.write_json(first_hook / "a.json", {"text": "FIRST-DEFAULT"})
        self.write_json(first_hook / "b.json", {
            "10": {"text": "FIRST-TEN"},
            "50": [{"text": "FIRST-B-ONE"}, {"text": "FIRST-B-TWO"}],
            "80": {"text": "FIRST-EIGHTY"},
        })
        self.write_json(self.second / "extensions/hook", {
            "10": {"text": "SECOND-TEN"},
            "50": {"text": "SECOND-FIFTY"},
        })
        self.write(self.first / "ordinary", "ORDINARY-FIRST")
        self.write(self.second / "ordinary", "ORDINARY-SECOND")

        result = self.dry_show()

        self.assert_success(result)
        self.assert_in_order(
            result.stdout,
            "START",
            "FIRST-TEN",
            "SECOND-TEN",
            "FIRST-DEFAULT",
            "FIRST-B-ONE",
            "FIRST-B-TWO",
            "SECOND-FIFTY",
            "FIRST-EIGHTY",
            "ORDINARY-FIRST",
            "END",
        )
        self.assertNotIn("ORDINARY-SECOND", result.stdout)

    def test_missing_path_is_a_noop_but_found_invalid_inputs_are_errors(self):
        self.make_build([
            {"text": "START"},
            {"search-merge": "extensions/hook"},
            {"text": "END"},
        ])

        missing = self.dry_show()

        self.assert_success(missing)
        self.assert_in_order(missing.stdout, "START", "END")

        invalid_cases = (
            ("malformed JSON", lambda: self.write(self.second / "extensions/hook", "{broken")),
            ("invalid contribution", lambda: self.write_json(
                self.second / "extensions/hook", {"50": "RUN echo not-an-entry"})),
            ("invalid entry value", lambda: self.write_json(
                self.second / "extensions/hook", {"text": 42})),
            ("non-JSON directory child", lambda: self.write(
                self.second / "extensions/hook/README", "not JSON")),
            ("nested directory", lambda: (self.second / "extensions/hook/nested").mkdir(
                parents=True)),
        )
        for label, prepare in invalid_cases:
            with self.subTest(label=label):
                target = self.second / "extensions/hook"
                if target.is_file():
                    target.unlink()
                elif target.is_dir():
                    for child in sorted(target.rglob("*"), reverse=True):
                        child.rmdir() if child.is_dir() else child.unlink()
                    target.rmdir()
                prepare()

                result = self.dry_show()

                self.assertNotEqual(result.returncode, 0)
                self.assertIn(str(target), result.stderr)

    def test_nested_entries_use_fragment_origin_and_resolve_docsuez_dependencies(self):
        self.make_build([
            {"text": "START"},
            {"search-merge": "extensions/top.json"},
            {"text": "END"},
        ])
        top = self.second / "extensions/top.json"
        self.write_json(top, [
            {"relative-path": "relative.Dockerfile"},
            {"bash-print-text": "printf 'RUN origin=%s\\n' \"$PWD\""},
            {"search-merge": "extensions/nested.json"},
        ])
        self.write(top.parent / "relative.Dockerfile", "RELATIVE-CONTENT")
        self.write_json(self.first / "extensions/nested.json", [
            {"from-docsuez": "dep", "as": "builder"},
            {"copy-from-docsuez": "dep", "src": "/tool", "dst": "/bin/"},
        ])
        self.make_build({"text": "FROM scratch"}, name="dep")

        result = self.dry_show()

        self.assert_success(result)
        self.assert_in_order(
            result.stdout,
            "START",
            "RELATIVE-CONTENT",
            f"RUN origin={top.parent}",
            "FROM dep-image:latest AS builder",
            "COPY --from=dep-image:latest /tool /bin/",
            "END",
        )

    def test_nested_search_merge_cycle_is_an_error(self):
        self.make_build({"search-merge": "extensions/one.json"})
        one = self.first / "extensions/one.json"
        two = self.second / "extensions/two.json"
        self.write_json(one, {"search-merge": "extensions/two.json"})
        self.write_json(two, {"search-merge": "extensions/one.json"})

        result = self.dry_show()

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("cycle", result.stderr.lower())
        self.assertIn(str(one), result.stderr)
        self.assertIn(str(two), result.stderr)

    def test_imported_json_and_relative_files_make_an_existing_image_stale(self):
        self.make_build({"search-merge": "extensions/hook.json"})
        contribution = self.first / "extensions/hook.json"
        relative = self.first / "extensions/relative.Dockerfile"
        self.write_json(contribution, {"relative-path": "relative.Dockerfile"})
        self.write(relative, "FROM scratch")
        self.write_json(self.first / "env/run-docsuez.json", {"command": ["true"]})

        fake_bin = self.root / "bin"
        fake_bin.mkdir()
        docker_log = self.root / "docker.log"
        fake_docker = self.write(
            fake_bin / "docker",
            "#!/bin/sh\n"
            "if [ \"$1 $2\" = 'image inspect' ]; then\n"
            "  printf '%s\\n' '[{\"Id\":\"sha256:test\",\"Created\":\"2030-01-01T00:00:00Z\"}]'\n"
            "  exit 0\n"
            "fi\n"
            f"if [ \"$1\" = build ]; then printf '%s\\n' \"$*\" >> {docker_log}; fi\n"
            "exit 0\n",
        )
        fake_docker.chmod(0o755)
        env = dict(self.env, PATH=f"{fake_bin}{os.pathsep}{self.env['PATH']}")
        old = 1_700_000_000
        new = 2_000_000_000
        for path in (self.first / "env/build-docsuez.json", contribution, relative):
            os.utime(path, (old, old))

        current = self.docsuez("run", "env", "--no-default-addons", env=env)

        self.assert_success(current)
        self.assertFalse(docker_log.exists())

        os.utime(relative, (new, new))
        stale_relative = self.docsuez("run", "env", "--no-default-addons", env=env)

        self.assert_success(stale_relative)
        self.assertIn("build", docker_log.read_text())

        before = docker_log.read_text().count("build")
        os.utime(relative, (old, old))
        os.utime(contribution, (new, new))
        stale_json = self.docsuez("run", "env", "--no-default-addons", env=env)

        self.assert_success(stale_json)
        self.assertGreater(docker_log.read_text().count("build"), before)


if __name__ == "__main__":
    unittest.main()
