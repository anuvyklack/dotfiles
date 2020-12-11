# Command completion
# ========================================================================

setopt menu_complete    # При множестве вариатнов подстановки по нажатию
                        # <Tab> откроет меню и подставит первый вариант.
                        # При повторном нажатии подставит следующий
                        # вариант.  В меню можно пользоваться стрелками.

setopt complete_aliases # дополнять aliaces как отдельные команды

setopt list_types       # When listing files that are possible completions,
                        # show the type of each file with a trailing
                        # identifying mark.

unsetopt flow_control   # Disable start/stop characters in shell editor.

# unsetopt case_glob      # makes globbing (filename generation) case-sensitive

setopt always_to_end    # move cursor to the end of a completed word
setopt auto_list        # automatically list choices on ambiguous completion

setopt auto_param_slash # if completed parameter is a directory, add a trailing slash

setopt complete_in_word # complete from both ends of a word

# Next options EXTREMELY SLOWS DOWN COMPLETION
# setopt path_dirs        # perform path search even on command names with slashes

unsetopt glob_dots      # Dotfiles are matched without explicitly
                        # specifying the dot

setopt auto_menu        # Show completion menu on a successive tab press.

# correction
unsetopt correct_all
setopt correct

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1	# Because we didn't really complete anything
}

# Fuzzy matching of completions for when you mistype them:
# zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle -e ':completion:*' completer '
    if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]] ; then
        _last_try="$HISTNO$BUFFER$CURSOR"
        reply=(_complete _match _ignored _prefix _files)
    else
        if [[ $words[1] == (rm|mv) ]] ; then
            reply=(_complete _files)
        else
            reply=(_oldlist _expand _force_rehash _complete _ignored _correct _approximate _files)
        fi
    fi'

zstyle ':completion:*:match:*' original only


# Increase the number of errors based on the length of the typed word. But make
# sure to cap (at 7) the max-errors to avoid hanging.
zstyle -e ':completion:*:approximate:*' \
  max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'
# zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Выбирать предлагаемые zsh варианты автодополнения с помощью стрелочек.
zstyle ':completion:*' menu select=1 _complete _ignored _approximate

# Navigation in completion menu {{{

# use the vi navigation keys (hjkl) besides cursor keys in menu completion
zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char         # left
bindkey -M menuselect 'k' vi-up-line-or-history    # up
bindkey -M menuselect 'l' vi-forward-char          # right
bindkey -M menuselect 'j' vi-down-line-or-history  # down

bindkey -M menuselect '^f' vi-forward-word   # moves the mark one screenful down
bindkey -M menuselect '^b' vi-backward-word  # moves the mark one screenful up

bindkey -M menuselect 'gg' beginning-of-history  # moves the mark to the first line
bindkey -M menuselect 'G'  end-of-history        # moves the mark to the last line

# undo the completion and restore the previous content of the command line
bindkey -M menuselect '^[' send-break  # escape

# Shift-Tab to go back in completion menu
bindkey -M menuselect '^[[Z' reverse-menu-complete

# }}}

# Do not insert Tab when the are no characters to the left of the cursor
zstyle ':completion:*' insert-tab false

# # provide .. as a completion
# zstyle ':completion:*' special-dirs ..

# cd will never select the parent directory (e.g.: cd ../<TAB>):
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '(aptitude-*|*\~)'

# activate color-completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Directories
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'

# History

# ignore duplicate entries
zstyle ':completion:*:history-words'   remove-all-dups yes
zstyle ':completion:*:history-words'   stop yes

zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes


# Environment Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# If you end up using a directory as argument, this will remove the
# trailing slash (usefull in ln)
zstyle ':completion:*' squeeze-slashes true

# The following lines were added by compinstall
zstyle ':completion:*' verbose true
zstyle :compinstall filename "$ZDOTDIR/.zshrc"

# Уравниваем в правах верхний и нижний регистр
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
# zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Some functions, like _apt and _dpkg, are very slow. You can use a cache
# in order to proxy the list of results (like the list of available debian
# packages) Use a cache:
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh/.zcompcache

# # Use caching so that commands like apt and dpkg complete are useable
# zstyle ':completion::complete:*' use-cache 1
# zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"


# Completing process IDs with menu selection:
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always


# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'

# describe options in full
zstyle ':completion:*:options' description 'yes'

zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
# zstyle ':completion:*:descriptions' format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
# zstyle ':completion:*:warnings' format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes


zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# on processes completion complete all user processes
zstyle ':completion:*:processes' command 'ps -au$USER'


# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters


# Array completion element sorting.
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

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'


# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
zstyle ':completion:*:history-words' list false

# recent (as of Dec 2007) zsh versions are able to provide descriptions
# for commands (read: 1st word in the line) that it will list for the user
# to choose from. The following disables that, because it's not exactly fast.
zstyle ':completion:*:-command-:*:' verbose false

# define files to ignore for zcompile
zstyle ':completion:*:*:zcompile:*' ignored-patterns '(*~|*.zwc)'
zstyle ':completion:correct:' prompt 'correct to: %e'

# Ignore completion functions for commands you don't have:
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# Provide more processes in completion of programs like killall:
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# complete manual by their section
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

# Search path for sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
                                           /usr/local/bin  \
                                           /usr/sbin       \
                                           /usr/bin        \
                                           /sbin           \
                                           /bin            \
                                           /usr/X11R6/bin

# Speed up pasting w/ autosuggest
# # https://github.com/zsh-users/zsh-autosuggestions/issues/351
pasteinit() {
  OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
  zle -N self-insert url-quote-magic
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(bracketed-paste) # Clear suggestions on paste
}
pastefinish() {
  zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish
