#!/usr/bin/env zsh
#  ██               ███  ██                                          ██
# ░██              ░░██ ░░                                          ░██
# ░██████    █████  ░██  ██  ██   ██      ██████████    █████    ██████   █████
# ░██░░░██  ██░░░██ ░██ ░██ ░░██ ██      ░░██░░██░░██  ██░░░██  ██░░░██  ██░░░██
# ░██  ░██ ░███████ ░██ ░██  ░░███        ░██ ░██ ░██ ░██  ░██ ░██  ░██ ░███████
# ░██  ░██ ░██░░░░  ░██ ░██   ██░██       ░██ ░██ ░██ ░██  ░██ ░██  ░██ ░██░░░░
# ░██  ░██ ░░█████  ░██ ░██  ██ ░░██      ███ ░██ ░██ ░░█████  ░░██████ ░░█████
# ░░   ░░   ░░░░░   ░░  ░░  ░░   ░░      ░░░  ░░  ░░   ░░░░░    ░░░░░░   ░░░░░
#
# This file is loaded from `.zshrc` and configures `Multirious/zsh-helix-mode`.
#
# * Config

# ESC (^[) switches to normal mode, but it is also the prefix of escape
# sequences (arrow keys, etc.), so after ESC the line editor waits $KEYTIMEOUT
# (in hundredths of a second) for more bytes before acting. The zsh default is
# 40 (400ms); 10 (100ms) feels near-instant while leaving enough delay for
# multi-byte sequences.
KEYTIMEOUT=10

# Use steady (non-blinking) underline cursor in insert mode.
ZHM_CURSOR_INSERT=$'\e[0m\e[4 q\e]12;white\a'

# Integration with `zsh-syntax-highlighting`.
zhm-add-update-region-highlight-hook

# Integration with `zsh-autosuggestions`.
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(
  zhm_history_prev
  zhm_history_next
  zhm_prompt_accept
  zhm_accept
  zhm_accept_or_insert_newline
)
ZSH_AUTOSUGGEST_ACCEPT_WIDGETS+=(
  zhm_move_right
  zhm_clear_selection_move_right
)
ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS+=(
  zhm_move_next_word_start
  zhm_move_next_word_end
)

# * keys

# C-; — flip selection (native M-;)
bindkey -M hxnor '\e[59;5u' zhm_flip_selections

# C-S-v — fix "No such widget `.zhm_bracketed_paste'" error
#
# `input` zimfw module binds the `bracketed-paste` widget to
# `bracketed-paste-url-magic`, which internally runs `zle .$WIDGET` expecting
# $WIDGET to be `bracketed-paste`. helix-mode's `zhm_bracketed_paste` calls
# `zle bracketed-paste` WITHOUT `-w`, so $WIDGET stays `zhm_bracketed_paste`;
# url-magic then runs `zle .zhm_bracketed_paste` and aborts with: No such widget
# `.zhm_bracketed_paste'. Re-add `-w` so $WIDGET reflects the called widget.
# Patching the body (instead of editing the plugin file) to make it survive
# after `zimfw update`.
() {
  local fn=$(typeset -f zhm_bracketed_paste)
  eval "${fn/zle bracketed-paste/zle bracketed-paste -w}"
}

# ** Duplicate "mi" keys under "m" prefix

# Patch `zhm_select_surround_pair_inner` so wrappers can pass the bracket
# character via _ZHM_SURROUND_CHAR instead of through $KEYS (which is
# a read-only ZLE parameter; regular shell variables use dynamic scoping).
() {
  local fn old new
  fn=$(typeset -f zhm_select_surround_pair_inner)
  old='local char="${KEYS:2}"'
  new='local char="${_ZHM_SURROUND_CHAR:-${KEYS:2}}"'
  eval "${fn/$old/$new}"
}

# m( m)
function _zhm_select_paren_inner {
  local _ZHM_SURROUND_CHAR='('
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_paren_inner

# m[ m]
function _zhm_select_bracket_inner {
  local _ZHM_SURROUND_CHAR='['
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_bracket_inner

# m{ m}
function _zhm_select_brace_inner {
  local _ZHM_SURROUND_CHAR='{'
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_brace_inner

# m< m>
function _zhm_select_angle_inner {
  local _ZHM_SURROUND_CHAR='<'
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_angle_inner

# m'
function _zhm_select_squote_inner {
  local _ZHM_SURROUND_CHAR="'"
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_squote_inner

# m"
function _zhm_select_dquote_inner {
  local _ZHM_SURROUND_CHAR='"'
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_dquote_inner

# m`
function _zhm_select_bquote_inner {
  local _ZHM_SURROUND_CHAR='`'
  zhm_select_surround_pair_inner
}
zle -N _zhm_select_bquote_inner

bindkey -M hxnor 'mw' zhm_select_word_inner
bindkey -M hxnor 'mW' zhm_select_long_word_inner
bindkey -M hxnor 'm)' _zhm_select_paren_inner
bindkey -M hxnor 'm[' _zhm_select_bracket_inner
bindkey -M hxnor 'm]' _zhm_select_bracket_inner
bindkey -M hxnor 'm{' _zhm_select_brace_inner
bindkey -M hxnor 'm}' _zhm_select_brace_inner
bindkey -M hxnor 'm<' _zhm_select_angle_inner
bindkey -M hxnor 'm>' _zhm_select_angle_inner
bindkey -M hxnor "m'" _zhm_select_squote_inner
bindkey -M hxnor 'm"' _zhm_select_dquote_inner
bindkey -M hxnor 'm"' _zhm_select_dquote_inner
bindkey -M hxnor 'm`' _zhm_select_bquote_inner

# ** "gh", "gs", "gl" make selections

function _zhm_goto_line_start_sel {
  for i in {1..$#zhm_cursors_pos}; do
    local cursor=$zhm_cursors_pos[$i]
    __zhm_trailing_goto $i $(__zhm_find_line_start $cursor "$BUFFER") 0
  done
  __zhm_update_last_moved
  __zhm_update_region_highlight
}
zle -N _zhm_goto_line_start_sel


function _zhm_goto_line_end_sel {
  for i in {1..$#zhm_cursors_pos}; do
    local cursor=$zhm_cursors_pos[$i]
    local line_end=$(__zhm_find_line_end $cursor "$BUFFER")
    local goto=
    if [[
      "$BUFFER[$((line_end + 1))]" == $'\n'
      && "$BUFFER[$line_end]" != $'\n'
    ]]; then
      goto=$((line_end - 1))
    else
      goto=$line_end
    fi
    __zhm_trailing_goto $i $goto 0
  done
  __zhm_update_last_moved
  __zhm_update_region_highlight
}
zle -N _zhm_goto_line_end_sel


function _zhm_goto_line_first_nonwhitespace_sel {
  setopt localoptions rematchpcre
  for i in {1..$#zhm_cursors_pos}; do
    local cursor=$zhm_cursors_pos[$i]
    local line_start=$(__zhm_find_line_start $cursor "$BUFFER")
    if [[ "${BUFFER:$line_start}" =~ '\A[^\S\n]*' ]]; then
      __zhm_trailing_goto $i $((line_start + MEND)) 0
    fi
  done
  __zhm_update_last_moved
  __zhm_update_region_highlight
}
zle -N _zhm_goto_line_first_nonwhitespace_sel


bindkey -M hxnor 'gh' _zhm_goto_line_start_sel
bindkey -M hxnor 'gl' _zhm_goto_line_end_sel
bindkey -M hxnor 'gs' _zhm_goto_line_first_nonwhitespace_sel

# * .

# Local Variables:
# eval: (outli-mode 1)
# outline-blank-line: nil
# End:
