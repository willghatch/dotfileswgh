#!/usr/bin/zsh

# nuke viins map, replacing it with the contents of the emacs map
bindkey -A emacs viins

# escapes for command mode
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode

# use viins, which is now emacs-y, but has an out to vicmd mode
bindkey -v


