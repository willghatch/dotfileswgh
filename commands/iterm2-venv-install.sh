#!/usr/bin/env bash

vdir=~/dotfileswgh-dotlocal/iterm2-python3-venv

python3 -m venv $vdir
$vdir/bin/pip install iterm2
