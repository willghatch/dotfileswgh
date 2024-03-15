#!/bin/sh
# Adapted from https://stackoverflow.com/questions/11505255/osx-check-if-the-screen-is-locked

function screenIsLocked { [ "$(/usr/libexec/PlistBuddy -c "print :IOConsoleUsers:0:CGSSessionScreenIsLocked" /dev/stdin 2>/dev/null <<< "$(ioreg -n Root -d1 -a)")" = "true" ]; }

if screenIsLocked; then
    echo "locked"
else
    echo "unlocked"
fi

