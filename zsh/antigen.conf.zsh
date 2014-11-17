
source $DOTFILESDIR/external/zsh/antigen/antigen.zsh

antigen bundles <<EOBUNDLES
    zsh-users/zsh-history-substring-search
    hchbaw/opp.zsh.git
    # TODO - get opp's sub-pieces working (surround, between)
    #hchbaw/auto-fu.zsh.git
    alfredodeza/zsh-plugins.git vi #vi visual
    willghatch/zsh-snippets
    # use the oh-my-zsh version of wd because the upstream does crap like install in ~/bin
    wd
    zsh-users/zsh-syntax-highlighting
    cabal
    catimg
    # the cp plugin provides cpv - rsync based progress-showing cp
    cp
EOBUNDLES


if [[ -d ~/vzsh ]]; then
    antigen bundle ~/vzsh --no-local-clone
    #antigen bundle ~/vzsh zaw-sources --no-local-clone
else
    antigen bundle willghatch/vzsh.git
    #antigen bundle willghatch/vzsh.git zaw-sources
fi

antigen bundle zsh-users/zaw
antigen bundle willghatch/vzsh.git zaw-sources
#antigen theme vzsh

antigen apply
