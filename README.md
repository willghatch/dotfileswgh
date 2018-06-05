dotfileswgh
===========

This git repository is for my personal configuration files.

It uses [stow](https://www.gnu.org/software/stow/) to symlink configuration files in place.  Everything linked by stow is in the `homedir` directory.

My main emacs configuration is in emacs/def.el.  Just because.  My vim and emacs configurations are ...mostly in sync.  But I give more love to my emacs config.

One big point I'm trying to do is to create common keybindings for all the programs I use.  My plan for harmonizing everything that does windowing (window managers proper, like xmonad, dwm, etc., as well as tmux, vim, emacs... even firefox with pentadactyl has some similar functionality) is in the windowkeys file.

If you have any questions, comments, or suggestions let me know!

Be warned -- I do weird stuff and experiment with things a lot.  You may find things that you think are crazy.

If you want to try it out, the linkup script hooks everything up to my liking, basically by running stow, making some directories I always use if they don't exist, and `chmod`ing .cache to be private.  If you don't want to link stuff (understandable), emacs and vim can be started with my config without fuss with <code>vim -u $DOTFILESWGH/vimrc</code> and <code>emacs -l $DOTFILESWGH/emacs/def.el</code>, but they require at least the minimal environment settings in the env-basic.sh file.  Note that emacs needs a ton of packages, so first I run $DOTFILESWGH/emacs/install.sh to install them.


