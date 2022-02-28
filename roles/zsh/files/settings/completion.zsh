# https://thevaluable.dev/zsh-completion-guide-examples/

#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
#  Shell completion settings
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# For more information open 'man zshoptions' and search for “Completion”.

setopt menu_complete    # При множестве вариатнов подстановки по нажатию
                        # <Tab> откроет меню и подставит первый вариант.
                        # При повторном нажатии подставит следующий
                        # вариант. В меню можно пользоваться стрелками.

# setopt auto_menu        # Show completion menu on a successive tab press (two
#                         # use of the Tab key).  This option is overridden by
#                         # MENU_COMPLETE.

# setopt auto_complete    # Select the first match given by the completion menu.
#                         # Override AUTO_MENU.

# setopt list_rows_first  # Matches are sorted in rows instead of columns.

setopt complete_aliases # Дополнять aliaces как отдельные команды.

setopt list_types       # When listing files that are possible completions,
                        # show the type of each file with a trailing
                        # identifying mark.

unsetopt flow_control   # Disable start/stop characters in shell editor.

# unsetopt case_glob      # makes globbing (filename generation) case-sensitive

setopt always_to_end    # Move cursor to the end of a completed word.
setopt list_packed      # The completion menu takes less space.
setopt auto_list        # Automatically list choices on ambiguous completion.

setopt auto_param_slash # If completed parameter is a directory, add a trailing slash.

setopt complete_in_word # By default, the cursor goes at the end of the word
                        # when completion start. Setting this will not move the
                        # cursor and the completion will happen on both end of
                        # the word completed.

setopt glob_complete    # Trigger the completion after a glob * instead of
                        # expanding it.

# setopt path_dirs        # This options EXTREMELY SLOWS DOWN COMPLETION.
#                         # Perform path search even on command names with slashes.

unsetopt glob_dots      # Dotfiles are matched without explicitly specifying
                        # the dot.


#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
#  zstyle
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# The general way to use zstyle to configure a Zsh module:
#
#     zstyle <pattern> <style> <values>
#
# zstyle pattern for the completion:
#
#     :completion:<function>:<completer>:<command>:<argument>:<tag>
#
# completion  - String acting as a namespace, to avoid pattern collisions with
#               other scripts also using zstyle.
# <function>  - Apply the style to the completion of an external function or
#               widget.
# <completer> - Apply the style to a specific completer. We need to drop the
#               underscore from the completer’s name here.
# <command>   - Apply the style to a specific command, like cd, rm, or sed for
#               example.
# <argument>  - Apply the style to the n-th option or the n-th argument.
#               It’s not available for many styles.
# <tag>       - Apply the style to a specific tag. You can think of a tag as a
#               type of match. For example “files”, “domains”, “users”, or
#               “options” are tags.
#
# $ man zshcompsys
# List of styles -- search for “Standard Styles”.
# List of tags -- search for “Standard Tags”.
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

zstyle ':completion:*' verbose true

# Define completers
zstyle ':completion:*' completer _extensions _complete _approximate


#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
#  Menu
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# Use the menu to select zsh completion suggestions.
zstyle ':completion:*' menu select=1
# zstyle ':completion:*' menu select=1 _complete _ignored _approximate

# Navigation in completion menu {{{

# Use the vi navigation keys (hjkl) besides cursor keys in menu completion.
zmodload zsh/complist  # Should be called before compinit.

bindkey -M menuselect 'h' vi-backward-char         # left
bindkey -M menuselect 'k' vi-up-line-or-history    # up
bindkey -M menuselect 'l' vi-forward-char          # right
bindkey -M menuselect 'j' vi-down-line-or-history  # down

bindkey -M menuselect '^f' vi-forward-word   # moves one screenful down
bindkey -M menuselect '^b' vi-backward-word  # moves one screenful up

bindkey -M menuselect 'gg' beginning-of-history  # moves to the first line
bindkey -M menuselect 'G'  end-of-history        # moves to the last line

# Undo the completion and restore the previous content of the command line.
bindkey -M menuselect '^[' send-break  # escape

# Shift-Tab to go back in completion menu
bindkey -M menuselect '^[[Z' reverse-menu-complete

# }}}
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

# Some functions, like _apt and _dpkg, are very slow. It is possible to use a
# cache in order to proxy the list of results (like the list of available debian
# packages).
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path $HOME/.cache/zsh/.zcompcache

# This style is used by _expand_alias function. Set to complete the aliases.
zstyle ':completion:*' complete true

# Do not insert Tab when the are no characters to the left of the cursor.
zstyle ':completion:*' insert-tab false

# Autocomplete options for cd instead of directory stack
zstyle ':completion:*' complete-options true

# In which order sort files on completion.
# Possible values: size, link, time, modification, access.
zstyle ':completion:*' file-sort alphabetically

# # provide .. as a completion
# zstyle ':completion:*' special-dirs ..

# cd will never select the parent directory (e.g.: cd ../<TAB>):
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Don't complete backup files as executables.
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '(aptitude-*|*\~)'

# Enable filename colorizing according to ls colors for zsh completion.
zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}

# Directories
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'

# History
# Ignore duplicate entries.
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes


# Environment Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# If you end up using a directory as argument, this will remove the
# trailing slash (usefull in ln)
zstyle ':completion:*' squeeze-slashes true

# Уравниваем в правах верхний и нижний регистр
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'

# Completing process IDs with menu selection:
zstyle ':completion:*:kill:*' force-list always

# Disable sort when completing `git checkout`.
zstyle ':completion:*:git-checkout:*' sort false

#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
#  Group matches and describe.
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# Categorize completion suggestions with headings.
# Required for completion to be in good groups (named after the tags)
zstyle ':completion:*' group-name ''

zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands

# Style the group names
# ---------------------
#                underline              end bold
#                v                      v
# $'%F{yellow}%B%U%{\e[3m%}%d%{\e[23m%}%b%u%f'
#              ^  ^~~~~~~~~  ^~~~~~~~~~   ^
#           bold  italic     end italic   end underline
#
# zstyle ':completion:*' format $' %F{yellow}%B%{\e[3m%}%d%{\e[23m%}%b%f'

# describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

zstyle ':completion:*:*:*:*:corrections'  format 'F{green}!- %d (errors: %e) -!%f'
zstyle ':completion:*:*:*:*:descriptions' format '%F{blue}%B-- %D %d --%b%f'
zstyle ':completion:*:*:*:*:messages'     format ' %F{yellow} -- %d --%f'
zstyle ':completion:*:*:*:*:warnings'     format ' %F{magenta}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――


zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# On processes completion complete all user processes.
zstyle ':completion:*:processes' command 'ps -au$USER'

# Array completion element sorting.
# Offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti \
        canna clamav daemon dbus distcache dnsmasq dovecot fax ftp \
        games gdm gkrellmd gopher hacluster haldaemon halt hsqldb ident \
        junkbust kdm ldap lp mail mailman mailnull man messagebus \
        mldonkey mysql nagios named netdump news nfsnobody nobody nscd \
        ntp nut nx obsrun openvpn operator pcap polkitd postfix \
        postgres privoxy pulse pvm quagga radvd rpc rpcuser rpm rtkit \
        scard shutdown squid sshd statd svn sync tftp usbmux uucp vcsa \
        wwwrun xfs '_*'
# ... unless we really want to.
zstyle '*' single-ignored show

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Man. Complete manual by their section.
zstyle ':completion:*:manuals'       separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'


# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
zstyle ':completion:*:history-words' list false

# # recent (as of Dec 2007) zsh versions are able to provide descriptions
# # for commands (read: 1st word in the line) that it will list for the user
# # to choose from. The following disables that, because it's not exactly fast.
# zstyle ':completion:*:-command-:*:' verbose false

# define files to ignore for zcompile
zstyle ':completion:*:*:zcompile:*' ignored-patterns '(*~|*.zwc)'
zstyle ':completion:correct:' prompt 'correct to: %e'

# Ignore completion functions for commands you don't have:
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# Provide more processes in completion of programs like killall:
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# Search path for sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
                                           /usr/local/bin  \
                                           /usr/sbin       \
                                           /usr/bin        \
                                           /sbin           \
                                           /bin            \
                                           /usr/X11R6/bin


#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
#  Corrections
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

# Run rehash on completion so new installed program are found automatically:
_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1  # Because we didn't really complete anything
}

# Some people don't like the automatic correction - so add
# 'export NO_CORRECTIONS=1' in '.zprofile' to deactivate it.
if [[ -n "$NO_CORRECTIONS" ]]
then
  zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete _files
  setopt nocorrect      # do not try to correct the spelling if possible
else
  setopt correct        # Correct the spelling of commands.

  # Fuzzy matching of completions for when you mistype them:
  zstyle -e ':completion:*' completer '
    if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]]
    then
      _last_try="$HISTNO$BUFFER$CURSOR"
      reply=(_complete _match _ignored _prefix _files)
    else
      if [[ $words[1] == (rm|mv) ]]
      then
        reply=(_complete _files)
      else
        reply=(_oldlist _expand _force_rehash _complete _ignored _correct _approximate _files)
      fi
    fi'
fi

# 'original' style is used by the '_approximate' and '_correct' completers to
# decide if the original string should be added as a  possible completion.
zstyle ':completion:*' original true
# zstyle ':completion:*:match:*' original only


# The style 'max-errors' is used by the '_approximate' and '_correct' completer
# functions to determine the maximum number of errors to allow.
#
#   zstyle ':completion:*:approximate:*' max-errors 2 numeric
#
# Increase the number of errors based on the length of the typed word. But make
# sure to cap (at 7) the max-errors to avoid hanging.
zstyle -e ':completion:*:approximate:*' \
  max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'


# vim: tw=80 ts=2 sw=2 fdm=marker
