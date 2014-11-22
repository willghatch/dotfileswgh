
source $DOTFILESDIR/external/zsh/antigen/antigen.zsh

antigen bundles <<EOBUNDLES
    zsh-users/zsh-history-substring-search
    hchbaw/opp.zsh.git
    # TODO - get opp's sub-pieces working (surround, between)
    #hchbaw/auto-fu.zsh.git
    alfredodeza/zsh-plugins.git vi #vi visual
    zsh-users/zaw
    willghatch/zsh-snippets
    willghatch/zsh-zaw-extras
    tarrasch/zsh-colors
    tarrasch/zsh-bd
    tarrasch/zsh-functional
    voronkovich/gitignore.plugin.zsh
    jocelynmallon/zshmarks
    zsh-users/zsh-syntax-highlighting
    skx/sysadmin-util # adds cool commands (adds to path)
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


antigen apply
