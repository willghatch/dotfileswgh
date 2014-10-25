
autoload -U colors && colors


umask 077

# no c-s/c-q output freezing
setopt noflowcontrol
# allow expansion in prompts
setopt prompt_subst
# this is default, but set for share_history
setopt append_history
# import new commands from the history file also in other zsh-session
#setopt share_history
# save each command's beginning timestamp and the duration to the history file
setopt extended_history
# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list
#setopt histignorealldups
# remove command lines from the history list when the first character on the
# line is a space
#setopt histignorespace
# if a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the cd command to that directory.
#setopt auto_cd
# in order to use #, ~ and ^ for filename generation grep word
# *~(*.gz|*.bz|*.bz2|*.zip|*.Z) -> searches for word not in compressed files
# don't forget to quote '^', '~' and '#'!
setopt extended_glob
# display PID when suspending processes as well
setopt longlistjobs
# try to avoid the 'zsh: no matches found...'
setopt nonomatch
# report the status of backgrounds jobs immediately
setopt notify
# whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all
# not just at the end
setopt completeinword
# Don't send SIGHUP to background processes when the shell exits.
#setopt nohup
# make cd push the old directory onto the directory stack.
#setopt auto_pushd
# avoid "beep"ing
#setopt nobeep
# don't push the same dir twice.
#setopt pushd_ignore_dups
# * shouldn't match dotfiles. ever.
#setopt noglobdots
# use zsh style word splitting
setopt noshwordsplit
# don't error out when unset parameters are used
setopt unset


# mailchecks
#MAILCHECK=30
# report about cpu-/system-/user-time of command if running longer than this
REPORTTIME=5
# watch for everyone but me and root
#watch=(notme root)
# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath




# make help (m-h) be more helpful
unalias run-help
autoload run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk

# make hooking possible
autoload -U add-zsh-hook
#add-zsh-hook [-dD] <hook> <function>
#hook is one of chpwd, periodic, precmd, preexec, zshaddhistory, zshexit, or zsh_directory_name

# recent directory list
# use with cdr command
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-file $VZSH_RECENT_DIRS_DIR/rd-${TTY##*/} +

