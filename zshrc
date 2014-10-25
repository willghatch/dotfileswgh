#!/usr/bin/zsh

source $HOME/dotfileswgh/bazsh/common.sh
source $DOTFILESDIR/zsh/vzshrc
fpath=($DOTFILESDIR/external/zsh/wd $fpath $HROOT/build/zsh-completions/src)
wd() {
    source $DOTFILESDIR/external/zsh/wd/wd.sh
}

tf=$DOTFILESDIR/external/zsh/opp.zsh/opp.zsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESDIR/external/zsh/opp.zsh/opp/textobj-between.zsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESDIR/external/zsh/opp.zsh/opp/surround.zsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESDIR/external/zsh/zle_vi_visual.zsh
if [ -f "$tf" ]
then
    source $tf
fi
source $DOTFILESDIR/zsh/highlight.zsh
tf=$DOTFILESDIR/external/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
if [ -f "$tf" ]
then
    source $tf
fi

tf=$DOTFILESLOCALDIR/bazsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESLOCALDIR/zshrc
if [ -f "$tf" ]
then
    source $tf
fi
unset tf

if [ -x ~/vscripts/motd.sh ]
then
    ~/vscripts/motd.sh
fi

