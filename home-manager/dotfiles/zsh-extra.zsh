# #
# # Misc extra functions and aliases:
# #
# # Function to cd and ls in one command
cl() {
    if [ "$#" -eq 0 ]; then
        "cd" || return
    else
        "cd" "$1" || return
    fi
    ls -A --color=auto
}

bindkey -s '^o' 'lfcd\n' # zsh

# some functions copied from the fzf github page https://github.com/junegunn/fzf/wiki/Examples
# fh - repeat history
fh() {
    print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf --height 50% --reverse --border +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

## cd on exiting lf
lfcd() {
    tmp="$(mktemp)"
    # `command` is needed in case `lfcd` is aliased to `lf`
    command lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}
##
## From https://gitlab.com/TheOuterLinux/Command-Line/-/blob/master/System/Terminals%20and%20Muxinators/bashrc/bashrc%20-%20Basic.txt
extract() {
    if [ -f "$1" ]; then
        case "$1" in
            *.tar.bz2) tar xvjf "$1" ;;
            *.tar.gz) tar xvzf "$1" ;;
            *.bz2) bunzip2 "$1" ;;
            *.rar) unrar x "$1" ;;
            *.gz) gunzip "$1" ;;
            *.tar) tar xvf "$1" ;;
            *.tbz2) tar xvjf "$1" ;;
            *.tgz) tar xvzf "$1" ;;
            *.zip) unzip "$1" ;;
            *.Z) uncompress "$1" ;;
            *.7z) 7z x "$1" ;;
            *) echo "I don't know how to extract $1..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}
#Get the mime information (file-type) of a file
#For example, a plain text file would have 'text/plain' as the mime
#
#    Usage: mime /path/to/file.ext
#
mime() { file --mime-type "$1"; }
