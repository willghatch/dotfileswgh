# output the repos to my dotlocal dir
ADOTDIR=$DOTFILESWGH/dotlocal/antigen
source $DOTFILESWGH/external/zsh/antigen/antigen.zsh

antigen bundles <<EOBUNDLES
    zsh-users/zsh-history-substring-search
    hchbaw/opp.zsh.git
    #hchbaw/opp.zsh.git opp
    alfredodeza/zsh-plugins.git vi #vi visual
    zsh-users/zaw
    willghatch/zsh-zaw-extras

    willghatch/zsh-hooks
    willghatch/zsh-megaprompt
    willghatch/zsh-snippets
    willghatch/zsh-grml-funcs
    willghatch/vzsh

    tarrasch/zsh-colors
    tarrasch/zsh-bd
    tarrasch/zsh-functional

    voronkovich/gitignore.plugin.zsh
    jocelynmallon/zshmarks
    ehamberg/zsh-cabal-completion
    skx/sysadmin-util # adds cool commands (adds to path)
    zsh-users/zsh-syntax-highlighting
EOBUNDLES


if [[ -d ~/vzsh ]]; then
    antigen bundle ~/vzsh --no-local-clone
    #antigen bundle ~/vzsh zaw-sources --no-local-clone
else
    antigen bundle willghatch/vzsh.git
    #antigen bundle willghatch/vzsh.git zaw-sources
fi


antigen apply
