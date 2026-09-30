#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    echo "Usage: $0"
    echo "Launch startup programs for a Floatile session: the eww bar, everything in wayland-startup-programs.sh, then each floatile/floatile-startup-programs.sh in \$XDG_CONFIG_DIRS."
    echo "Run by startup-commands in the Floatile configuration."
    exit 0
fi

# The eww bar replaces Waybar, which cannot show Floatile's workspaces or windows.
#waybar-configured &
floatile-eww-bar &

wayland-startup-programs.sh

# Per-machine extensions: every floatile/floatile-startup-programs.sh in
# $XDG_CONFIG_DIRS (default /etc/xdg), in path order, each directory once, as
# the Floatile configuration does for floatile/config.scm.  Each runs to
# completion, so it should background long-running programs.  Relative entries
# are ignored, as the XDG base directory spec says.
declare -A seen
IFS=: read -r -a dirs <<< "${XDG_CONFIG_DIRS:-/etc/xdg}"
for dir in "${dirs[@]}"; do
    [[ "$dir" == /* ]] || continue
    [[ "$dir" =~ ^(.*[^/])/*$ ]] && dir="${BASH_REMATCH[1]}"
    [[ -n "${seen[$dir]:-}" ]] && continue
    seen[$dir]=1
    script="$dir/floatile/floatile-startup-programs.sh"
    if [[ -f "$script" ]]; then
        bash "$script"
    fi
done
