#!/usr/bin/env zsh

# a bunch of stuff scraped from grml zshrc, because I wanted to use some but not all of it

#### TODO --- check out grml's accept-line() function and related stuff
#### TODO --- check out grml's interactive comments and how they work...
#### TODO --- grml sometimes has stuff like help-show-abk that maybe give interactive help...
    # learn about it
#### TODO -- look at help_zle_parse_keybindings() -- it may be emacs-like key help
#### TODO -- look at how the 'key' assoc. array is used (binds eg. home/delete/arrows to terminfo sequences)
#### TODO -- look at grml H-Glob function
#### TODO -- look at grml ssl-cert junk
#### TODO -- look at grml simple-extract
#### TODO -- go through all the commented out stuff and give it more consideration -- I stuck it in
     #because I thought I would want it, or at least want to think about it, but didn't want to make a final decision yet...




# just type '...' to get '../..'
rationalise-dot() {
local MATCH
if [[ $LBUFFER =~ '(^|/| | |'$'\n''|\||;|&)\.\.$' ]]; then
  LBUFFER+=/
  zle self-insert
  zle self-insert
else
  zle self-insert
fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
# without this, typing a . aborts incremental history search
bindkey -M isearch . self-insert

## get top 10 shell commands:
alias top10commands='print -l ${(o)history%% *} | uniq -c | sort -nr | head -n 10'

## Handy functions for use with the (e::) globbing qualifier (like nt)
# use like this:
# ls *(e:'nt /foo/bar':) # list all files newer than /foo/bar
contains() { grep -q "$*" $REPLY } # contains pattern
sameas() { diff -q "$*" $REPLY &>/dev/null } # same as file
ot () { [[ $REPLY -ot ${~1} ]] } # older than
nt() {
    if [[ -n $1 ]] ; then
        local NTREF=${~1}
    fi
    [[ $REPLY -nt $NTREF ]]
} # newer than

## get_ic() - queries imap servers for capabilities; real simple. no imaps
#ic_get() {
#    emulate -L zsh
#    local port
#    if [[ ! -z $1 ]] ; then
#        port=${2:-143}
#        print "querying imap server on $1:${port}...\n";
#        print "a1 capability\na2 logout\n" | nc $1 ${port}
#    else
#        print "usage:\n  $0 <imap-server> [port]"
#    fi
#}

## List all occurrences of programm in current PATH
# grml calls it plap
whiches() {
    emulate -L zsh
    if [[ $# = 0 ]] ; then
        echo "Usage:    $0 program"
        echo "Example:  $0 zsh"
        echo "Lists all occurrences of program in the current PATH."
    else
        ls -l ${^path}/*$1*(*N)
    fi
}

## Find out which libs define a symbol
########## This is broken -- but is a cool idea
#lcheck() {
#    if [[ -n "$1" ]] ; then
#        nm -go /usr/lib/lib*.a 2>/dev/null | grep ":[[:xdigit:]]\{8\} . .*$1"
#    else
#        echo "Usage: lcheck <function>" >&2
#    fi
#}


## Download a file and display it locally
#uopen() {
#    emulate -L zsh
#    if ! [[ -n "$1" ]] ; then
#        print "Usage: uopen \$URL/\$file">&2
#        return 1
#    else
#        FILE=$1
#        MIME=$(curl --head $FILE | \
#               grep Content-Type | \
#               cut -d ' ' -f 2 | \
#               cut -d\; -f 1)
#        MIME=${MIME%$'\r'}
#        curl $FILE | see ${MIME}:-
#    fi
#}

## Memory overview
# This doesn't seem useful...
#memusage() {
#    ps aux | awk '{if (NR > 1) print $5;
#                   if (NR > 2) print "+"}
#                   END { print "p" }' | dc
#}

## print hex value of a number
hex() {
    emulate -L zsh
    if [[ -n "$1" ]]; then
        printf "%x\n" $1
    else
        print 'Usage: hex <number-to-convert>'
        return 1
    fi
}


## associate types and extensions (be aware with perl scripts and anwanted behaviour!)
#check_com zsh-mime-setup || { autoload zsh-mime-setup && zsh-mime-setup }
#alias -s pl='perl -S'

## Some quick Perl-hacks aka /useful/ oneliner
#bew() { perl -le 'print unpack "B*","'$1'"' }
#web() { perl -le 'print pack "B*","'$1'"' }
#hew() { perl -le 'print unpack "H*","'$1'"' }
#weh() { perl -le 'print pack "H*","'$1'"' }
#pversion()    { perl -M$1 -le "print $1->VERSION" } # i. e."pversion LWP -> 5.79"
#getlinks ()   { perl -ne 'while ( m/"((www|ftp|http):\/\/.*?)"/gc ) { print $1, "\n"; }' $* }
#gethrefs ()   { perl -ne 'while ( m/href="([^"]*)"/gc ) { print $1, "\n"; }' $* }
#getanames ()  { perl -ne 'while ( m/a name="([^"]*)"/gc ) { print $1, "\n"; }' $* }
#getforms ()   { perl -ne 'while ( m:(\</?(input|form|select|option).*?\>):gic ) { print $1, "\n"; }' $* }
#getstrings () { perl -ne 'while ( m/"(.*?)"/gc ) { print $1, "\n"; }' $*}
#getanchors () { perl -ne 'while ( m/«([^«»\n]+)»/gc ) { print $1, "\n"; }' $* }
#showINC ()    { perl -e 'for (@INC) { printf "%d %s\n", $i++, $_ }' }
#vimpm ()      { vim `perldoc -l $1 | sed -e 's/pod$/pm/'` }
#vimhelp ()    { vim -c "help $1" -c on -c "au! VimEnter *" }



# utility functions
# this function checks if a command exists and returns either true
# or false. This avoids using 'which' and 'whence', which will
# avoid problems with aliases for which on certain weird systems. :-)
# Usage: check_com [-c|-g] word
# -c only checks for external commands
# -g does the usual tests and also checks for global aliases
check_com() {
    emulate -L zsh
    local -i comonly gatoo
    if [[ $1 == '-c' ]] ; then
        (( comonly = 1 ))
        shift
    elif [[ $1 == '-g' ]] ; then
        (( gatoo = 1 ))
    else
        (( comonly = 0 ))
        (( gatoo = 0 ))
    fi
    if (( ${#argv} != 1 )) ; then
        printf 'usage: check_com [-c] <command>\n' >&2
        return 1
    fi
    if (( comonly > 0 )) ; then
        [[ -n ${commands[$1]} ]] && return 0
        return 1
    fi
    if [[ -n ${commands[$1]} ]] \
           || [[ -n ${functions[$1]} ]] \
           || [[ -n ${aliases[$1]} ]] \
           || [[ -n ${reswords[(r)$1]} ]] ; then
        return 0
    fi
    if (( gatoo > 0 )) && [[ -n ${galiases[$1]} ]] ; then
        return 0
    fi
    return 1
}


for mod in parameter complist deltochar mathfunc ; do
    zmodload -i zsh/${mod} 2>/dev/null || print "Notice: no ${mod} available :("
done

#zmodload -a zsh/stat zstat
#zmodload -a zsh/zpty zpty
#zmodload -ap zsh/mapfile mapfile





### jump behind the first word on the cmdline.
### useful to add options.
function jump_after_first_word() {
    local words
    words=(${(z)BUFFER})
    if (( ${#words} <= 1 )) ; then
        CURSOR=${#BUFFER}
    else
        CURSOR=${#${words[1]}}
    fi
}
zle -N jump_after_first_word

#f5# Create directory under cursor or the selected area
inPlaceMkDirs() {
    # Press ctrl-xM to create the directory under the cursor or the selected area.
    # To select an area press ctrl-@ or ctrl-space and use the cursor.
    # Use case: you type "mv abc ~/testa/testb/testc/" and remember that the
    # directory does not exist yet -> press ctrl-XM and problem solved
    local PATHTOMKDIR
    if ((REGION_ACTIVE==1)); then
        local F=$MARK T=$CURSOR
        if [[ $F -gt $T ]]; then
            F=${CURSOR}
            T=${MARK}
        fi
        # get marked area from buffer and eliminate whitespace
        PATHTOMKDIR=${BUFFER[F+1,T]%%[[:space:]]##}
        PATHTOMKDIR=${PATHTOMKDIR##[[:space:]]##}
    else
        local bufwords iword
        bufwords=(${(z)LBUFFER})
        iword=${#bufwords}
        bufwords=(${(z)BUFFER})
        PATHTOMKDIR="${(Q)bufwords[iword]}"
    fi
    [[ -z "${PATHTOMKDIR}" ]] && return 1
    PATHTOMKDIR=${~PATHTOMKDIR}
    if [[ -e "${PATHTOMKDIR}" ]]; then
        zle -M " path already exists, doing nothing"
    else
        zle -M "$(mkdir -p -v "${PATHTOMKDIR}")"
        zle end-of-line
    fi
}
zle -N inPlaceMkDirs


# grep for running process, like: 'any vim'
any() {
    emulate -L zsh
    unsetopt KSH_ARRAYS
    if [[ -z "$1" ]] ; then
        echo "any - grep for process(es) by keyword" >&2
        echo "Usage: any <keyword>" >&2 ; return 1
    else
        ps xauwww | grep -i "${grep_options[@]}" "[${1[1]}]${1[2,-1]}"
    fi
}




# aliases from grml
# general
##a2# Execute \kbd{du -sch}
#alias da='du -sch'
##a2# Execute \kbd{jobs -l}
#alias j='jobs -l'
## listing stuff
##a2# Execute \kbd{ls -lSrah}
#alias dir="ls -lSrah"
##a2# Only show dot-directories
#alias lad='ls -d .*(/)'
##a2# Only show dot-files
#alias lsa='ls -a .*(.)'
##a2# Only files with setgid/setuid/sticky flag
#alias lss='ls -l *(s,S,t)'
##a2# Only show symlinks
#alias lsl='ls -l *(@)'
##a2# Display only executables
#alias lsx='ls -l *(*)'
##a2# Display world-{readable,writable,executable} files
#alias lsw='ls -ld *(R,W,X.^ND/)'
##a2# Display the ten biggest files
#alias lsbig="ls -flh *(.OL[1,10])"
##a2# Only show directories
#alias lsd='ls -d *(/)'
##a2# Only show empty directories
#alias lse='ls -d *(/^F)'
##a2# Display the ten newest files
#alias lsnew="ls -rtlh *(D.om[1,10])"
##a2# Display the ten oldest files
#alias lsold="ls -rtlh *(D.Om[1,10])"
##a2# Display the ten smallest files
#alias lssmall="ls -Srl *(.oL[1,10])"
##a2# Display the ten newest directories and ten newest .directories
#alias lsnewdir="ls -rthdl *(/om[1,10]) .*(D/om[1,10])"
##a2# Display the ten oldest directories and ten oldest .directories
#alias lsolddir="ls -rthdl *(/Om[1,10]) .*(D/Om[1,10])"
## some useful aliases
##a2# Remove current empty directory. Execute \kbd{cd ..; rmdir \$OLDCWD}
#alias rmcdir='cd ..; rmdir $OLDPWD || cd $OLDPWD'
##a2# ssh with StrictHostKeyChecking=no \\&\quad and UserKnownHostsFile unset
#alias insecssh='ssh -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"'
##a2# scp with StrictHostKeyChecking=no \\&\quad and UserKnownHostsFile unset
#alias insecscp='scp -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"'


#f5# Backup \kbd{file {\rm to} file\_timestamp}
bk() {
    emulate -L zsh
    cp -b $1 $1_`date --iso-8601=m`
}
#f5# cd to directoy and list files
cl() {
    emulate -L zsh
    cd $1 && ls -a
}
# smart cd function, allows switching to /etc when running 'cd /etc/fstab'
cd() {
    if (( ${#argv} == 1 )) && [[ -f ${1} ]]; then
        [[ ! -e ${1:h} ]] && return 1
        print "Correcting ${1} to ${1:h}"
        builtin cd ${1:h}
    else
        builtin cd "$@"
    fi
}
#f5# List files which have been accessed within the last {\it n} days, {\it n} defaults to 1
accessed() {
    emulate -L zsh
    print -l -- *(a-${1:-1})
}
#f5# List files which have been changed within the last {\it n} days, {\it n} defaults to 1
changed() {
    emulate -L zsh
    print -l -- *(c-${1:-1})
}
#f5# List files which have been modified within the last {\it n} days, {\it n} defaults to 1
modified() {
    emulate -L zsh
    print -l -- *(m-${1:-1})
}


#f5# Change the xterm title from within GNU-screen
xtrename() {
    emulate -L zsh
    if [[ $1 != "-f" ]] ; then
        if [[ -z ${DISPLAY} ]] ; then
            printf 'xtrename only makes sense in X11.\n'
            return 1
        fi
    else
        shift
    fi
    if [[ -z $1 ]] ; then
        printf 'usage: xtrename [-f] "title for xterm"\n'
        printf ' renames the title of xterm from _within_ screen.\n'
        printf ' also works without screen.\n'
        printf ' will not work if DISPLAY is unset, use -f to override.\n'
        return 0
    fi
    print -n "\eP\e]0;${1}\C-G\e\\"
    return 0
}

insert-datestamp() { LBUFFER+=${(%):-'%D{%Y-%m-%d}'}; }
zle -N insert-datestamp
insert-last-typed-word() { zle insert-last-word -- 0 -1 }
zle -N insert-last-typed-word


_complete_screen_display() {
    [[ "$TERM" != "screen" && -z "$TMUX" ]] && return 1

    local TMPFILE=$(mktemp)
    local -U -a _screen_display_wordlist
    trap "rm -f $TMPFILE" EXIT

    # fill array with contents from screen hardcopy
    if ((${+TMUX})); then
        #works, but crashes tmux below version 1.4
        #luckily tmux -V option to ask for version, was also added in 1.4
        tmux -V &>/dev/null || return
        tmux -q capture-pane \; save-buffer -b 0 $TMPFILE \; delete-buffer -b 0
    else
        screen -X hardcopy $TMPFILE
        # screen sucks, it dumps in latin1, apparently always. so recode it
        # to system charset
        check_com recode && recode latin1 $TMPFILE
    fi
    _screen_display_wordlist=( ${(QQ)$(<$TMPFILE)} )
    # remove PREFIX to be completed from that array
    _screen_display_wordlist[${_screen_display_wordlist[(i)$PREFIX]}]=""
    compadd -a _screen_display_wordlist
}

help-glob() {
    zle -M "
    /      directories
    .      plain files
    @      symbolic links
    =      sockets
    p      named pipes (FIFOs)
    *      executable plain files (0100)
    %      device files (character or block special)
    %b     block special files
    %c     character special files
    r      owner-readable files (0400)
    w      owner-writable files (0200)
    x      owner-executable files (0100)
    A      group-readable files (0040)
    I      group-writable files (0020)
    E      group-executable files (0010)
    R      world-readable files (0004)
    W      world-writable files (0002)
    X      world-executable files (0001)
    s      setuid files (04000)
    S      setgid files (02000)
    t      files with the sticky bit (01000)

  print *(m-1)          # Files modified up to a day ago
  print *(a1)           # Files accessed a day ago
  print *(@)            # Just symlinks
  print *(Lk+50)        # Files bigger than 50 kilobytes
  print *(Lk-50)        # Files smaller than 50 kilobytes
  print **/*.c          # All *.c files recursively starting in \$PWD
  print **/*.c~file.c   # Same as above, but excluding 'file.c'
  print (foo|bar).*     # Files starting with 'foo' or 'bar'
  print *~*.*           # All Files that do not contain a dot
  chmod 644 *(.^x)      # make all plain non-executable files publically readable
  print -l *(.c|.h)     # Lists *.c and *.h
  print **/*(g:users:)  # Recursively match all files that are owned by group 'users'
  echo /proc/*/cwd(:h:t:s/self//) # Analogous to >ps ax | awk '{print $1}'<"
}
run-help-glob(){
    help-glob
}
zle -N run-help-glob

