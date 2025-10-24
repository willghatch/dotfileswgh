#!/usr/bin/env bash

cd ../iree-build

# the test suite is run with `ctest`, and you can target a specific test, eg `ctest -R emit_debug_info -V`, but it must be run in the build dir

ctest "$@"
