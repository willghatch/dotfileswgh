"""Behavioral tests for ordered docsuez pre-tmux scripts."""

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


COMMANDS = Path(__file__).resolve().parent.parent
DOCSUEZ = COMMANDS / "docsuez"


class PreTmuxScriptTest(unittest.TestCase):
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

    def write_config(self, name, data):
        path = self.config_dir / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data))

    def dry_show(self, *options):
        result = subprocess.run(
            [sys.executable, str(DOCSUEZ), "run", "env", "--build", "no",
             "--dry-show", "--session-name", "pre-tmux-test", *options],
            cwd=self.root,
            env=self.env,
            text=True,
            capture_output=True,
            timeout=5,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result.stdout.split("INNER_SCRIPT=$(cat <<'DOCSUEZ_INNER_TSUEZ_LAUNCH_SCRIPT'\n", 1)[1].split(
            "DOCSUEZ_INNER_TSUEZ_LAUNCH_SCRIPT", 1)[0]

    def test_numeric_positions_order_base_default_and_explicit_addon_lines(self):
        self.write_config("env/run-docsuez.json", {
            "image-name": "test:latest",
            "pre-tmux-script": {"30": "echo BASE-30", "60": "echo BASE-60"},
        })
        self.write_config("setup.default.run-docsuez.json", {
            "pre-tmux-script": {"10": "echo DEFAULT-10", "60": "echo DEFAULT-60"},
        })
        self.write_config("extra.run-docsuez.json", {
            "pre-tmux-script": {"20": "echo ADDON-20", "60": "echo ADDON-60"},
        })

        script = self.dry_show("--addon", "extra")

        expected = ["echo DEFAULT-10", "echo ADDON-20", "echo BASE-30",
                    "echo BASE-60", "echo DEFAULT-60", "echo ADDON-60"]
        for line in expected:
            self.assertIn(line, script)
        positions = [script.index(line) for line in expected]
        self.assertEqual(positions, sorted(positions))

    def test_legacy_string_and_list_share_position_fifty_and_defaults_can_be_omitted(self):
        self.write_config("env/run-docsuez.json", {
            "image-name": "test:latest", "pre-tmux-script": "echo BASE-50",
        })
        self.write_config("setup.default.run-docsuez.json", {
            "pre-tmux-script": ["echo DEFAULT-50"],
        })
        self.write_config("extra.run-docsuez.json", {
            "pre-tmux-script": {"50": "echo ADDON-50"},
        })

        script = self.dry_show("--addon", "extra")
        expected = ["echo BASE-50", "echo DEFAULT-50", "echo ADDON-50"]
        for line in expected:
            self.assertIn(line, script)
        positions = [script.index(line) for line in expected]
        self.assertEqual(positions, sorted(positions))

        without_defaults = self.dry_show("--addon", "extra", "--no-default-addons")
        self.assertIn("echo BASE-50", without_defaults)
        self.assertIn("echo ADDON-50", without_defaults)
        self.assertNotIn("echo DEFAULT-50", without_defaults)


if __name__ == "__main__":
    unittest.main()
