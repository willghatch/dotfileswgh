#!/bin/sh

set -eu

package_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec emacs -Q --batch \
     -L "$package_dir" \
     -l table-highlight-tests.el \
     -f ert-run-tests-batch-and-exit
