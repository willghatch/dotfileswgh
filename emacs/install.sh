#!/usr/bin/env sh

df=$DOTFILESWGH

usage() {
    cat <<'EOF'
Usage: install.sh [OPTION] [ARGS...]

Options:
  (none), --install      Install emacs packages (default)
  --freeze               Update the straight.el lockfile, merging with existing entries
                         (new hashes win on conflict; old entries for absent repos kept)
  --freeze-remove-unused Update the lockfile replacing it entirely with currently-cloned
                         repos, removing entries for any repos no longer present
  --full-clone           Install packages using full (non-shallow) git clones
  --straight-update-packages PKG...
                         Update the named packages to their latest upstream commit,
                         unshallowing any shallow repos first (does not freeze)
  --print-hashes PKG...  Print the current git HEAD commit for each named package,
                         i.e. what --freeze would write to the lockfile for them
  --help                 Show this help text
EOF
}

mode=install

case "$1" in
    ''|--install)
        ;;
    --freeze)
        mode=freeze
        ;;
    --full-clone)
        mode=full-clone
        ;;
    --freeze-remove-unused)
        mode=freeze-remove-unused
        ;;
    --straight-update-packages)
        mode=update-packages
        shift
        ;;
    --print-hashes)
        mode=print-hashes
        shift
        ;;
    --help|-h)
        usage
        exit 0
        ;;
    *)
        printf 'Unknown option: %s\n' "$1" >&2
        usage >&2
        exit 1
        ;;
esac

emacs -batch -l "$df/emacs/install.el" -- "$mode" "$@"
