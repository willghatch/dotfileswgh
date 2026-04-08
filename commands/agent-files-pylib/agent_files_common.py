"""
agent_files_common - Shared utilities for agent-files tools

Provides common functionality used by agents-md-generate, df-skills,
and potentially other tools:
  - Git root discovery
  - Dev-types gathering (CLI + env + JSON, deduplicated)
  - XDG-based search path construction
  - State file reading/writing
"""

import json
import os
import sys
from pathlib import Path


def find_git_root():
    """Find the root of the current git repository, or None.

    When AGENT_WORK_GIT_ROOT is set, uses that directly.
    Otherwise walks up from cwd looking for .git.  If .git is a file
    (worktree or submodule), follows the gitdir reference to find the
    real .git directory and returns its parent.
    """
    env_root = os.environ.get("AGENT_WORK_GIT_ROOT")
    if env_root:
        return Path(env_root)
    cwd = Path.cwd()
    for candidate in [cwd] + list(cwd.parents):
        dot_git = candidate / ".git"
        if dot_git.is_dir():
            return candidate
        if dot_git.is_file():
            # Worktree or submodule: .git is a file containing "gitdir: <path>"
            text = dot_git.read_text().strip()
            if text.startswith("gitdir:"):
                gitdir = Path(text.split(":", 1)[1].strip())
                if not gitdir.is_absolute():
                    gitdir = (candidate / gitdir).resolve()
                # gitdir points to e.g. /repo/.git/worktrees/NAME
                # Walk up to find the actual .git directory
                real_git_dir = gitdir
                while real_git_dir.name != ".git" and real_git_dir.parent != real_git_dir:
                    real_git_dir = real_git_dir.parent
                if real_git_dir.name == ".git":
                    return real_git_dir.parent
            return candidate
    return None


def read_dev_types_json(git_root, prog_name="agent-files"):
    """Read dev types from .git/agent-files/dev-types.json if it exists.

    Returns a list of strings, or an empty list if the file doesn't exist
    or is malformed.  prog_name is used in warning messages.
    """
    if not git_root:
        return []
    dev_types_file = git_root / ".git" / "agent-files" / "dev-types.json"
    if not dev_types_file.is_file():
        return []
    try:
        with open(dev_types_file) as f:
            data = json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        print(
            f"{prog_name}: warning: failed to read '{dev_types_file}': {e}",
            file=sys.stderr,
        )
        return []
    if not isinstance(data, list):
        print(
            f"{prog_name}: warning: '{dev_types_file}' is not a JSON list, ignoring",
            file=sys.stderr,
        )
        return []
    result = []
    for item in data:
        if isinstance(item, str):
            result.append(item)
        else:
            print(
                f"{prog_name}: warning: non-string entry in '{dev_types_file}', ignoring: {item!r}",
                file=sys.stderr,
            )
    return result


def gather_dev_types(cli_dev_types, git_root, prog_name="agent-files"):
    """Combine dev types from CLI flags, $DEV_TYPES env var, and dev-types.json.

    Priority: cli_dev_types (highest), then $DEV_TYPES env var, then JSON file.
    All sources are merged and deduplicated preserving order.

    Returns a list of dev type strings.
    """
    dev_types = list(cli_dev_types)
    for env_dt in os.environ.get("DEV_TYPES", "").split(","):
        env_dt = env_dt.strip()
        if env_dt and env_dt not in dev_types:
            dev_types.append(env_dt)
    for json_dt in read_dev_types_json(git_root, prog_name):
        if json_dt and json_dt not in dev_types:
            dev_types.append(json_dt)
    return dev_types


def get_base_search_paths(config_name, dev_types=(), git_root=None):
    """Return ordered list of base paths that each contain default/ and non-default/ subdirs.

    config_name is the tool-specific directory name (e.g. "agents-md-generate"
    or "df-skills").

    Priority: git root > XDG_CONFIG_HOME > XDG_CONFIG_DIRS.
    Within each location, dev-type-specific paths precede the base path.
    """
    paths = []

    def add_location(base):
        for dt in dev_types:
            paths.append(base / dt)
        paths.append(base)

    # Git repo path (most specific to current project)
    if git_root:
        add_location(git_root / ".git" / "agent-files" / "config" / config_name)

    home = Path.home()

    # XDG_CONFIG_HOME (user-personal)
    xdg_config_home = os.environ.get("XDG_CONFIG_HOME", str(home / ".config"))
    add_location(Path(xdg_config_home) / config_name)

    # XDG_CONFIG_DIRS (system/shared)
    xdg_config_dirs = os.environ.get("XDG_CONFIG_DIRS", "/etc/xdg")
    for config_dir in xdg_config_dirs.split(":"):
        if config_dir:
            add_location(Path(config_dir) / config_name)

    # De-duplicate while preserving order
    seen = set()
    deduped = []
    for path in paths:
        resolved = path.resolve()
        if resolved not in seen:
            seen.add(resolved)
            deduped.append(path)

    return deduped


def get_raw_location_bases(config_name, git_root=None):
    """Return ordered list of raw location bases (without dev-type expansion).

    Used to discover available dev-type directory names.
    """
    bases = []

    if git_root:
        bases.append(git_root / ".git" / "agent-files" / "config" / config_name)

    home = Path.home()
    xdg_config_home = os.environ.get("XDG_CONFIG_HOME", str(home / ".config"))
    bases.append(Path(xdg_config_home) / config_name)

    xdg_config_dirs = os.environ.get("XDG_CONFIG_DIRS", "/etc/xdg")
    for config_dir in xdg_config_dirs.split(":"):
        if config_dir:
            bases.append(Path(config_dir) / config_name)

    return bases


LINK_PREFIX = "link:"


def find_link_files(base_paths, subdir):
    """Find link files in the given subdirectory across all base paths.

    Link files are empty files named ``link:ITEM_NAME`` inside a subdir
    (typically ``default/``).  They promote the named item -- which must
    exist somewhere in the opposite category (``non-default/`` when the
    link is in ``default/``, and vice versa) -- into the category where
    the link appears.

    Returns an ordered dict of item_name -> link_file_path, with earlier
    base paths taking priority (first found wins).  Only link files whose
    names start with the ``link:`` prefix are considered.
    """
    links = {}
    for base_path in base_paths:
        link_dir = base_path / subdir
        if not link_dir.is_dir():
            continue
        for entry in sorted(link_dir.iterdir()):
            if entry.is_file() and entry.name.startswith(LINK_PREFIX):
                target_name = entry.name[len(LINK_PREFIX):]
                if target_name and target_name not in links:
                    links[target_name] = entry
    return links


def resolve_links(items, links, donor_items, prog_name="agent-files"):
    """Merge link-promoted items into an existing item dict.

    ``items`` is the dict of name -> path for items already found in the
    target category (e.g. default segments/skills).

    ``links`` is the dict of link_name -> link_file_path returned by
    :func:`find_link_files` for the same category.

    ``donor_items`` is the dict of name -> path for items in the opposite
    category (e.g. non-default segments/skills) from which linked items
    are resolved.

    For each link whose target name is not already present in ``items``,
    this function looks up the target in ``donor_items`` and, if found,
    adds it to ``items``.  If the target is not found, a warning is
    printed to stderr.

    Returns ``items`` (mutated in place for convenience).
    """
    for target_name, link_path in links.items():
        if target_name in items:
            # Already present in the target category (real item takes
            # priority over a link).
            continue
        if target_name in donor_items:
            items[target_name] = donor_items[target_name]
        else:
            print(
                f"{prog_name}: warning: link '{link_path.name}' in "
                f"'{link_path.parent}' targets '{target_name}' which "
                f"was not found in any search path",
                file=sys.stderr,
            )
    return items


def get_state_file_path(config_name, git_root):
    """Return the path to the state file, or None if not in a git repo."""
    if not git_root:
        return None
    return git_root / ".git" / "agent-files" / "config" / config_name / "state.json"


def load_state(state_path):
    """Load state from JSON file.  Returns dict or None if file doesn't exist."""
    if state_path and state_path.is_file():
        with open(state_path) as f:
            return json.load(f)
    return None


def save_state(state_path, dev_types, added, omitted):
    """Save state to JSON file, creating directories as needed."""
    state_path.parent.mkdir(parents=True, exist_ok=True)
    state = {
        "version": 1,
        "dev_types": list(dev_types),
        "added": list(added),
        "omitted": list(omitted),
    }
    with open(state_path, "w") as f:
        json.dump(state, f, indent=2)
        f.write("\n")
