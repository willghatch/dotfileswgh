#!/bin/sh
echo source this file, eg. "source \$(which $0)", dont execute it
# must source this, not run it
source ../iree-build/.env && export PYTHONPATH
