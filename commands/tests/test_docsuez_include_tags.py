"""Behavioral tests for tags in run configs loaded through "include"."""

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


COMMANDS = Path(__file__).resolve().parent.parent
DOCSUEZ = COMMANDS / "docsuez"


class IncludeTagsTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.config_dir = self.root / "configs"
        self.config_dir.mkdir()
        self.env = dict(os.environ)
        self.env.update({
            "DOCSUEZ_PATH": str(self.config_dir),
            "PATH": f"{COMMANDS}{os.pathsep}{self.env['PATH']}",
            "XDG_RUNTIME_DIR": str(self.root / "runtime"),
            "TSUEZ_TMUX_CONF": str(self.root / "missing-tmux.conf"),
        })
        # The shared config that variants include.
        self.write_config("base/run-docsuez.json", {
            "image-name": "test:latest",
            "tags": {
                "gpu": {"docker-args": ["--label=BASE-GPU"]},
                "groups": {"docker-args": ["--label=BASE-GROUPS"]},
            },
        })

    def write_config(self, name, data):
        path = self.config_dir / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data))

    def dry_show(self, config, *options):
        result = subprocess.run(
            [sys.executable, str(DOCSUEZ), "run", config, "--build", "no",
             "--dry-show", "--session-name", "include-tags-test", *options],
            cwd=self.root,
            env=self.env,
            text=True,
            capture_output=True,
            timeout=5,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result.stdout

    def test_explicit_addon_replaces_tag_from_included_config(self):
        self.write_config("variant/run-docsuez.json", {"include": ["base"]})
        self.write_config("no-gpu.run-docsuez.json", {"tags": {"gpu": {}}})

        output = self.dry_show("variant", "-a", "no-gpu")

        # The addon's empty "gpu" tag must win over the included one...
        self.assertNotIn("BASE-GPU", output)
        # ...while included tags it does not name still apply.
        self.assertIn("BASE-GROUPS", output)

    def test_including_config_tag_replaces_same_named_included_tag(self):
        self.write_config("variant/run-docsuez.json", {
            "include": ["base"],
            "tags": {"gpu": {"docker-args": ["--label=VARIANT-GPU"]}},
        })

        output = self.dry_show("variant")

        self.assertIn("VARIANT-GPU", output)
        self.assertNotIn("BASE-GPU", output)
        self.assertIn("BASE-GROUPS", output)


if __name__ == "__main__":
    unittest.main()
