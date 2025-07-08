# Requires $HOME and $DOTFILESWGH be set

export DOTFILESWGH="${DOTFILESWGH:-$HOME/dotfileswgh}"
source $DOTFILESWGH/env-more.sh

# source other dotfileswgh mixin envs if they exist
if [ -f "$DOTFILESWGH_GHP/env.sh" ]; then
    source "$DOTFILESWGH_GHP/env.sh"
fi
if [ -f "$DOTFILESWGH_PRI/env.sh" ]; then
    source "$DOTFILESWGH_PRI/env.sh"
fi
if [ -f "$DOTFILESWGH_ROOTGIT/env.sh" ]; then
    source "$DOTFILESWGH_ROOTGIT/env.sh"
fi
if [ -f "$DOTFILESWGH_DOTLOCAL/env.sh" ]; then
    source "$DOTFILESWGH_DOTLOCAL/env.sh"
fi
if [ -f "$DOTFILESWGH_PRI_DOTLOCAL/env.sh" ]; then
    source "$DOTFILESWGH_PRI_DOTLOCAL/env.sh"
fi
