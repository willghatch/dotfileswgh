dotfileswgh
===========

This git repository is for my personal configuration files.

My main emacs configuration is in emacs/def.el.  IE I generally run emacs with a wrapper that runs `emacs -l $DOTFILESWGH/emacs/def.el`.  The other notable thing that I do with emacs is that I have a suite of `premacs` scripts that I use for pre-loading emacs servers so that I can connect to a fresh emacs instance with low startup time.  IE I like the speed of using emacs in client/server configuration, but I like having an independent instance with each invocation to not have all of my buffers and state mixed up.

My vim configuration is much simpler -- at this point I use vim with no packages as my go-to editor for editing server configurations and such.  Basically editing in small environments where I don't want to bother installing all of my emacs packages.

One big point I try to do is to create common keybindings for all the programs I use.  
Window managers proper like awesomewm, terminal window managers like tmux, editors like vim and emacs, and tabbed gui programs like firefox and other web browsers, etc all have a lot of shared or conceptually similar functionality but each with completely different keybindings.
The outline for how I have roughly harmonized many of these is in the `windowkeys` file.
I would like to find more common things like this to abstract over to make it easier to memorize and have muscle-memory for a large number of keybindings across a large number of programs.

If you have any questions, comments, or suggestions let me know!

But ultimately, this is a constantly evolving bunch of code that I throw together to make my environment convenient for me across many devices.  It has lots of dead code, experimental configuration that never went anywhere that I never deleted, ugly hacks, etc.

