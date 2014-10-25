#!/usr/bin/zsh

source $HOME/dotfileswgh/bazsh/common.sh
source $DOTFILESDIR/zsh/setopt.zsh
fpath=($DOTFILESDIR/external/zsh/wd $fpath $HROOT/build/zsh-completions/src)
source $DOTFILESDIR/zsh/completion.zsh
source $DOTFILESDIR/zsh/prompt.zsh
source $DOTFILESDIR/zsh/keys-pre.zsh
wd() {
    source $DOTFILESDIR/external/zsh/wd/wd.sh
}
source $DOTFILESDIR/zsh/abbreviations.zsh
source $DOTFILESDIR/zsh/grml-funcs.zsh
source $DOTFILESDIR/zsh/funcs.zsh

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

source $DOTFILESDIR/zsh/keys-post.zsh

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

