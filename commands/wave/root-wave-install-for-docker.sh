#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    echo "usage: run this script in the wave dir, uses sudo to do install as root"
    exit 1
fi

# TODO - this script is good for installing wave as root in a docker image that already has an /opt/venv and that also has rust set up with /root/.cargo/bin/rustc.

# Eg. this works with this docker image: rocm/sgl-dev:v0.5.7-rocm700-mi35x-20260105

# The idea is that the docker image already has pytorch and other stuff set up in a way that works for some particular hardware, but I don't want to run as root any more than I have to.  So this is a setup that I can run each time I start the container that should get me to a working local wave install.
# I want to avoid running as root because (1) you get mixed file ownership which is annoying and regularly needs to be fixed (eg. git complains about it, or you want to delete things outside of the container), and (2) running as root makes it easy to accidentally edit the docker image environment in ways such that things work until you restart docker and then they are broken and you don't know why.
# So the idea is to run this script at the start of launching such a docker container consistently on each launch.  If an environment has scripted or documented that you run this script at the start, then I can use those annoying docker images with necessary python setup owned by root while avoiding (at least some of) the problems of running in docker as root.

OUTER_UID="$(id -u)"
OUTER_GID="$(id -g)"

cat <<EOF | sudo bash -s --
set -e
PATH="/root/.cargo/bin:/opt/venv/bin:/opt/rocm/bin:/bin:usr/bin"
source /opt/venv/bin/activate
pip install --upgrade pip
pip install -r requirements-iree-pinned.txt
#pip install -r requirements.txt

# the rust part of the install can fail if there are already old build things in here
rm -rf wave_lang/kernel/wave/scheduling/aplp/target/
pip install -e .
# try to avoid root ownership of files...
chown --recursive $OUTER_UID:$OUTER_GID wave_lang/kernel/wave/scheduling/aplp/target

EOF

