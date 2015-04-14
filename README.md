dotfileswgh
===========

This git repository is for my personal configuration files.

In the root directory are common dotfiles that go in the home directory, some other add-ons (I keep stuff that works for both bash and zsh in bazsh, for example), and some directories which are generally analagous to $HOME/.dirname.

My main emacs configuration is in emacs/def.el.  Just because.  My vim and emacs configurations are ...mostly in sync.  But I give more love to my emacs config.

One big point I'm trying to do is to create common keybindings for all the programs I use.  My plan for harmonizing everything that does windowing (window managers proper, like xmonad, dwm, etc., as well as tmux, vim, emacs... even firefox with pentadactyl has some similar functionality) is in the windowkeys file.

If you have any questions, comments, or suggestions let me know!

Be warned -- I do weird stuff and experiment with things a lot.  You may find things that you think are crazy.

If you want to try it out, the linkup.bash script hooks everything up to my liking, such as symlinking .zshrc, .vimrc...  Emacs and vim can be started with my config without fuss with <code>vim -u $DOTFILESWGH/vimrc</code> and <code>emacs -l $DOTFILESWGH/emacs/def.el</code>, but they require at least the minimal environment settings in the env-dotfileswgh.sh file.  Note that emacs needs a ton of packages, so first I run $DOTFILESWGH/emacs/install.sh to install them.


