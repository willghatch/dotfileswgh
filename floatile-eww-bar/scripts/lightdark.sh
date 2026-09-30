#!/bin/sh
# lightdark.sh -- print the lightdark-status state, `light' or `dark', and
# again each time it changes.
#
# lightdark-status keeps the state in ${LIGHTDARK_DIR:-/tmp/lightdark-dir}/lightdark.
# With inotifywait the file is watched; without it, it is read every 2 seconds.
dir="${LIGHTDARK_DIR:-/tmp/lightdark-dir}"
mkdir -p "$dir" 2>/dev/null
last=""
report() {
    # The file itself, since lightdark-status prefers $WGH_THEME_DARK_OR_LIGHT,
    # which a toggle does not change.
    now=$(cat "$dir/lightdark" 2>/dev/null || lightdark-status 2>/dev/null)
    if [ "$now" != "$last" ]; then
        echo "$now"
        last="$now"
    fi
}
report
if command -v inotifywait >/dev/null 2>&1; then
    while inotifywait -qq -e close_write,create,moved_to,delete "$dir"; do
        report
    done
fi
while :; do
    sleep 2
    report
done
