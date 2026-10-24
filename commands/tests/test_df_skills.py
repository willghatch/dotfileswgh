"""Public CLI tests for the human skill inventory."""

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


DF_SKILLS = Path(__file__).resolve().parent.parent / "df-skills"


class SkillInventoryTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.config = self.root / "config" / "df-skills"
        self.extra = self.root / "extra"
        self.direct = self.root / "direct"
        self.env = dict(os.environ)
        self.env.update({
            "XDG_CONFIG_HOME": str(self.config.parent),
            "XDG_CONFIG_DIRS": str(self.root / "system"),
            "DF_SKILLS_EXTRA_DIRS": str(self.extra),
            "DF_SKILLS_EXTRA_DIRS_DIRECT": str(self.direct),
            "DEV_TYPES": "active",
        })

    def run_cli(self, *args):
        return subprocess.run(
            [sys.executable, str(DF_SKILLS), *args], cwd=self.root,
            env=self.env, text=True, capture_output=True, timeout=5,
        )

    def skill(self, path):
        path.mkdir(parents=True, exist_ok=True)
        (path / "SKILL.md").write_text("---\ndescription: Example\n---\nContent\n")

    def test_inventory_is_discoverable_only_in_human_help(self):
        human = self.run_cli("--human-help")
        self.assertEqual(human.returncode, 0, human.stderr)
        self.assertIn("inventory", human.stdout)
        self.assertIn("agents should use", human.stdout)
        for args in [("--help",), (), ("unknown-command",), ("list", "--help")]:
            with self.subTest(args=args):
                result = self.run_cli(*args)
                self.assertNotIn("inventory", result.stdout + result.stderr)
        result = self.run_cli("inventory", "--help")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("dev-type", result.stdout)

    def test_inventory_covers_inactive_layers_and_links_without_expanding_list(self):
        self.skill(self.config / "default" / "base")
        self.skill(self.config / "active" / "non-default" / "optional")
        self.skill(self.config / "inactive" / "default" / "specialized")
        self.skill(self.config / "non-default" / "promoted")
        link_dir = self.config / "inactive" / "default"
        (link_dir / "link:promoted").touch()
        self.skill(self.extra / "other" / "non-default" / "extra")
        self.skill(self.root / "system" / "df-skills" / "system" / "default" / "shared")
        self.skill(self.direct / "direct-skill")
        self.skill(self.extra / "default" / "base")
        # Nested sub-skills and unrelated directories are not top-level skills.
        self.skill(self.config / "default" / "base" / "nested")
        (self.config / "inactive" / "default" / "empty").mkdir()
        program = self.extra / "other" / "default" / "program"
        program.mkdir(parents=True)
        (program / "skill.prog").write_text("#!/bin/sh\nexit 99\n")

        result = self.run_cli("inventory")
        self.assertEqual(result.returncode, 0, result.stderr)
        lines = [line.split(maxsplit=3) for line in result.stdout.splitlines()]
        self.assertEqual(lines[0], ["dev-type", "default/non-default", "skill-name", "location"])
        self.assertCountEqual(lines[1:], [
            ["-", "default", "base", str(self.config / "default" / "base")],
            ["-", "default", "base", str(self.extra / "default" / "base")],
            ["-", "default", "direct-skill", str(self.direct / "direct-skill")],
            ["-", "non-default", "promoted", str(self.config / "non-default" / "promoted")],
            ["active", "non-default", "optional", str(self.config / "active" / "non-default" / "optional")],
            ["inactive", "default", "specialized", str(link_dir / "specialized")],
            ["inactive", "default", "promoted", str(link_dir / "link:promoted")],
            ["other", "non-default", "extra", str(self.extra / "other" / "non-default" / "extra")],
            ["other", "default", "program", str(program)],
            ["system", "default", "shared", str(self.root / "system" / "df-skills" / "system" / "default" / "shared")],
        ])
        listed = self.run_cli("list", "--names-only")
        self.assertEqual(listed.returncode, 0, listed.stderr)
        self.assertIn("optional", listed.stdout)
        self.assertNotIn("specialized", listed.stdout)
        self.assertNotIn("extra", listed.stdout)

    def test_empty_inventory_still_has_headers(self):
        result = self.run_cli("inventory")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            result.stdout.split(),
            ["dev-type", "default/non-default", "skill-name", "location"],
        )

    def test_disclose_defaults_to_all_dev_types_with_strict_opt_out(self):
        self.skill(self.config / "inactive" / "default" / "specialized")
        self.skill(self.config / "inactive" / "default" / "specialized" / "child")
        for name in ("specialized", "specialized/child"):
            with self.subTest(name=name):
                strict = self.run_cli("disclose", "--strict-dev-types", name)
                self.assertNotEqual(strict.returncode, 0)
                normal = self.run_cli("disclose", name)
                self.assertEqual(normal.returncode, 0, normal.stderr)
                result = self.run_cli("disclose", "--all-dev-types", name)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("Content", result.stdout)
                self.assertIn("inactive", result.stdout)
        self.assertNotIn("specialized", self.run_cli("list", "--names-only").stdout)

    def test_disclose_searches_active_pass_before_inactive_pass(self):
        inactive = self.config / "inactive" / "default" / "shared"
        active = self.extra / "active" / "non-default" / "shared"
        self.skill(inactive)
        self.skill(active)
        for flags in ((), ("--all-dev-types",)):
            result = self.run_cli("disclose", *flags, "shared")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(str(active), result.stdout)

    def test_disclose_inactive_pass_follows_location_order_before_dev_type_order(self):
        first = self.config / "z-last" / "non-default" / "shared"
        self.skill(first)
        self.skill(self.extra / "a-first" / "default" / "shared")
        result = self.run_cli("disclose", "shared")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(str(first), result.stdout)

    def test_disclose_active_pass_follows_location_order_and_explicit_dev_type_order(self):
        first = self.config / "second" / "non-default" / "shared"
        self.skill(first)
        self.skill(self.extra / "first" / "default" / "shared")
        result = self.run_cli("-d", "first", "-d", "second", "disclose", "shared")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(str(first), result.stdout)
        preferred = self.config / "first" / "non-default" / "shared"
        self.skill(preferred)
        result = self.run_cli("-d", "first", "-d", "second", "disclose", "shared")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(str(preferred), result.stdout)

    def test_disclose_base_scope_precedes_inactive_layers(self):
        base = self.extra / "non-default" / "shared"
        self.skill(base)
        self.skill(self.config / "inactive" / "default" / "shared")
        result = self.run_cli("disclose", "shared")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(str(base), result.stdout)

    def test_disclose_modes_are_mutually_exclusive(self):
        result = self.run_cli("disclose", "--strict-dev-types", "--all-dev-types", "shared")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("not allowed", result.stderr)

    def test_inventory_names_only_preserves_names_and_deduplicates_copies(self):
        self.skill(self.config / "inactive" / "default" / "name with spaces")
        self.skill(self.extra / "inactive" / "default" / "name with spaces")
        self.skill(self.config / "default" / "base")
        result = self.run_cli("inventory", "--names-only")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.splitlines(), ["base", "name with spaces"])

    def test_all_dev_types_preserves_priority_within_a_layer(self):
        first = self.config / "inactive" / "default" / "shared"
        self.skill(first)
        self.skill(self.extra / "inactive" / "default" / "shared")
        alias = self.config / "z-alias" / "default"
        alias.mkdir(parents=True)
        (alias / "shared").symlink_to(first, target_is_directory=True)
        result = self.run_cli("disclose", "--all-dev-types", "shared")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(str(first), result.stdout)

    def test_all_dev_types_supports_base_and_explicit_directory_disclosure(self):
        base = self.config / "default" / "base"
        self.skill(base)
        for name in ("base", str(base)):
            result = self.run_cli("disclose", "--all-dev-types", name)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(str(base), result.stdout)

    def test_all_dev_types_flag_is_visible_only_in_human_help(self):
        normal = self.run_cli("disclose", "--help")
        self.assertEqual(normal.returncode, 0, normal.stderr)
        self.assertNotIn("--all-dev-types", normal.stdout)
        self.assertIn("--strict-dev-types", normal.stdout)
        human = self.run_cli("disclose", "--human-help")
        self.assertEqual(human.returncode, 0, human.stderr)
        self.assertIn("--all-dev-types", human.stdout)


if __name__ == "__main__":
    unittest.main()
