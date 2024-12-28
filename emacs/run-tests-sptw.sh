#!/bin/sh

# TODO - I would like to run tests without requiring everything.  The problem is that I need to require things, but paths for libraries are set in my config.  I need to set up some kind of testing wrapper for installing relevant packages, running tests, etc.

emacs -batch -l def.el -l ert -l sptw-test.el -f ert-run-tests-batch-and-exit
