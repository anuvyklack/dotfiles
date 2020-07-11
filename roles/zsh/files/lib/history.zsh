# Command history managment
# ========================================================================

# History environment variables
HISTFILE=${HOME}/.cache/zsh/zsh_history
HISTSIZE=110000  # Larger than $SAVEHIST for HIST_EXPIRE_DUPS_FIRST to work
SAVEHIST=100000

setopt bang_hist               # Treat the '!' character specially during expansion.
# setopt extended_history        # Write the history file in the ":start:elapsed;command" format.
setopt inc_append_history      # Write to the history file immediately, not when the shell exits.
# setopt share_history           # Synchronize history across shells
setopt hist_reduce_blanks      # Remove superfluous blanks before recording entry.
setopt hist_verify             # Don't execute immediately upon history expansion.
setopt hist_no_store           # Remove the 'history' (fc -i) command form the history list
setopt hist_reduce_blanks      # Remove superfluous blanks from each command line being added to the history list.
setopt prompt_subst            # Allow expansion in prompts

setopt hist_find_no_dups       # Do not display a line previously found.
setopt hist_ignore_dups        # Don't record an entry that was just recorded again.
setopt hist_ignore_space       # Don't record an entry starting with a space.
setopt hist_expire_dups_first  # Delete duplicates first when HISTFILE size exceeds HISTSIZE

# setopt hist_ignore_all_dups    # Delete old recorded entry if new entry is a duplicate.
# setopt hist_save_no_dups       # Don't write duplicate entries in the history file.

# Patterns that would not be stored in history
export HISTORY_IGNORE="(cd|ranger|r|exit|kill)"

# Not store failed commands into history
zshaddhistory() { whence ${${(z)1}[1]} >/dev/null || return 2 }
