if status is-interactive
    # Commands to run in interactive sessions

    # Disable greeting message
    set fish_greeting

    #---------------------------------------------------------------------------
    # Color theme tuning
    #---------------------------------------------------------------------------
    set tide_character_icon            
    set tide_character_vi_icon_default 
    set tide_character_vi_icon_visual  
    set tide_character_vi_icon_replace 

    set tide_character_color         dc5bfc #dc5bfc
    set tide_character_color_failure e06c75 #e06c75
    set tide_git_color_branch        d1a166 #d1a166

    #---------------------------------------------------------------------------
    # Emulates vim's cursor shape behavior
    #---------------------------------------------------------------------------
    # Set the normal and visual mode cursors to a block
    set fish_cursor_default block
    # Set the insert mode cursor to a line
    set fish_cursor_insert underscore
    # Set the replace mode cursors to an underscore
    set fish_cursor_replace_one underscore
    set fish_cursor_replace underscore
    # Set the external cursor to a line. The external cursor appears when
    # a command is started. The cursor shape takes the value of
    # fish_cursor_default when fish_cursor_external is not specified.
    set fish_cursor_external line
    # The following variable can be used to configure cursor shape in
    # visual mode, but due to fish_cursor_default, is redundant here
    set fish_cursor_visual block

    set -g fish_vi_force_cursor 1

    #---------------------------------------------------------------------------
    # Extra tools
    #---------------------------------------------------------------------------
    if type -q atuin
        atuin init fish --disable-up-arrow | source
    end

    if type -q zoxide
        zoxide init fish | source
    end

    #---------------------------------------------------------------------------
    # Keybindings
    #---------------------------------------------------------------------------
    # commandline -P :: Evaluates to true if the commandline is showing pager
    #                   contents, such as tab completions.

    # # Use Ctrl + hjkl to navigate pager.
    # bind -M insert \cj 'commandline -P; and down-or-search; or commandline -i j'
    # bind -M insert \ck 'commandline -P; and up-or-search; or commandline -i k'
    # bind -M insert \ch 'commandline -P; and commandline -f backward-char; or commandline -i h'
    # bind -M insert \cl 'commandline -P; and commandline -f forward-char; or commandline -i l'

    # / :: slash key starts search in completion pager (like in Vim)
    bind -M insert / 'commandline -P; and commandline -f pager-toggle-search; or commandline -i /'

    # Ctrl + o :: Open yazi file manager and cd on exit
    bind -M insert \cO 'yy; commandline -f repaint'

    #---------------------------------------------------------------------------
    # Aliases & Abbreviations
    #---------------------------------------------------------------------------
    abbr --add gs git status
    abbr --add gss git status -s
    abbr --add gd git diff
    abbr --add gg git graph

    alias mv='mv -v'
    alias cp='cp -vR' # рекурсивное копирование
    alias rm='rm -v'

    # Создание каталогов без коррекции и со всеми родительскими каталогами, если
    # они отсутствуют.
    alias mkdir='mkdir -vp'

    alias grep='grep --color=auto'
    alias wget='wget -c' # автоматическое продолжение при разрыве соединения

    alias tree="tree -I .git -I .github"

    if type -q nvim
        alias vi="nvim"
        alias vim="nvim"
    end

    # Вывод свободного и использованного дискового пространства
    # в "гуманистическом" представлении.
    alias df="df -h"
    alias du="du -h"

    if type -q eza
        alias ls="eza -F --group-directories-first"
        alias ll="eza -lF --group-directories-first --git"
    else
        alias ls="ls --color=auto --group-directories-first"
    end

end # status is-interactive

