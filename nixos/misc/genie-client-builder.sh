
# this is a bash script, but it does not use the #!.

# This sets up an environment, including putting coreutils in PATH
source $stdenv/setup
# environment includes:
# $out -- path to where output must go
# $src -- path to where source lives
# $PWD and $TMP are temporary directories (maybe the same)
# $NIX_BUILD_CORES and $NIX_STORE are available

sound_theme_freedesktop="$1"

cp -r $src ./genie-build
chmod --recursive u+w genie-build
cd genie-build
patchShebangs scripts/get-assets.sh
cat scripts/get-assets.sh | sed s!/usr/share/sounds/freedesktop!$sound_theme_freedesktop/share/sounds/freedesktop! > scripts/get-assets-patched.sh
chmod +x scripts/get-assets-patched.sh
./scripts/get-assets-patched.sh

meson --prefix $out ./build/
ninja -C ./build/
ninja -C ./build/ install


