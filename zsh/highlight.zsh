# Highlighter plugin config
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
tf=$DOTFILESDIR/external/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [ -f "$tf" ]
then
    source $tf
fi
# Highlighters main
ZSH_HIGHLIGHT_STYLES[default]="none"
ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=red,bold"
ZSH_HIGHLIGHT_STYLES[path]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[path_approx]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[path_prefix]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[globbing]="fg=blue,bold,strikethrough"
ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=yellow"
ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[precommand]="fg=green"
ZSH_HIGHLIGHT_STYLES[command]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[hashed-command]="fg=green"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[alias]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[assign]="fg=white,bold"
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]="fg=magenta"
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=yellow"
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]="fg=magenta,bold"
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]="fg=red"
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]="fg=red"
# Highlighters root
ZSH_HIGHLIGHT_STYLES[root]="standout"
# Highlighters cursor
ZSH_HIGHLIGHT_STYLES[cursor]="standout"
# Highlighters brackets
ZSH_HIGHLIGHT_STYLES[bracket-level-1]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-2]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-3]="fg=magenta,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-4]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[bracket-level-5]="fg=cyan,bold"
ZSH_HIGHLIGHT_STYLES[bracket-error]="fg=red,bold"
ZSH_HIGHLIGHT_STYLES[cursor-matchingbracket]="standout"
# Highlighters pattern
# TODO -- use this one, because it looks cool and helpful, but requires more setup