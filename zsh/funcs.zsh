help-keys() {
    local keys="$(bindkey -LM $CUR_KEYMAP)"
    echo "keys for $CUR_KEYMAP:\n$keys" | $PAGER
}
run-help-keys(){
    help-keys
}
zle -N run-help-keys
