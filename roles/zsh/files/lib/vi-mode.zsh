# Prezto/editor/init.zsh

# Vi style keybindigs

bindkey -v  # Use Vi style keybindings
# bindkey -e  # Use Emacs style keybindings

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward


# Don't wait too long after <Esc> to see if it's an arrow / function key
# Warning: Setting this too low can break some zsh functionality, eg:
# https://github.com/zsh-users/zsh-autosuggestions/issues/254#issuecomment-345175735
export KEYTIMEOUT=1
# export KEYTIMEOUT=30


# # Change the color and shape of the terminal cursor with:
# MODE_CURSOR_VICMD="green block"
# MODE_CURSOR_VIINS="#20d08a blinking bar"
# MODE_CURSOR_SEARCH="#ff00ff steady underline"


# # Change cursor shape for different vi modes.
# function zle-keymap-select {
#   if [[ ${KEYMAP} == vicmd ]] ||
#      [[ $1 = 'block' ]]; then
#     echo -ne '\e[1 q'
#
#   elif [[ ${KEYMAP} == main ]] ||
#        [[ ${KEYMAP} == viins ]] ||
#        [[ ${KEYMAP} = '' ]] ||
#        [[ $1 = 'beam' ]]; then
#     echo -ne '\e[5 q'
#   fi
# }
# zle -N zle-keymap-select
#
# zle-line-init() {
#     zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
#     echo -ne "\e[5 q"
# }
# zle -N zle-line-init
#
# # Use beam shape cursor on startup.
# echo -ne '\e[5 q'
# # Use beam shape cursor for each new prompt.
# preexec() { echo -ne '\e[5 q' ;}

