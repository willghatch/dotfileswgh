zaw-src-todoman() {
    local line
    candidates=()
    while read line; do
        candidates+="$line"
    done < <(todo list)
    actions=(zaw--todoman-show zaw--todoman-done)
    act_descriptions=("show" "done")
}

zaw--todoman-done(){
    todo done $(echo $1 | awk '{print $1}')
}

zaw--todoman-show(){
    zle -M "$(todo show $(echo $1 | awk '{print $1}'))"
}

zaw-register-src -n todoman zaw-src-todoman
