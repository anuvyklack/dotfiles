if status is-interactive
    # Commands to run in interactive sessions

    #---------------------------------------------------------------------------
    # Color theme tuning
    #---------------------------------------------------------------------------
    set -g tide_character_icon            
    set -g tide_character_vi_icon_default 
    set -g tide_character_vi_icon_visual  
    set -g tide_character_vi_icon_replace 

    set -g tide_character_color         dc5bfc #dc5bfc
    set -g tide_character_color_failure e06c75 #e06c75
    set -g tide_git_color_branch        d1a166 #d1a166

    # Syntax highlighting — OneDark (mirrors themes/OneDark.theme).
    # Set here rather than via `fish_config theme choose`: since fish 4.3
    # these are global (not universal) variables, so they must be set on
    # every shell start.  Variables left unset fall back to fish's own
    # defaults, which already match this theme.
    set -g fish_color_normal           abb2bf
    set -g fish_color_command          61afef
    set -g fish_color_quote            98c379
    set -g fish_color_redirection      56b6c2
    set -g fish_color_end              abb2bf
    set -g fish_color_error            e06c75
    set -g fish_color_param            abb2bf
    set -g fish_color_comment          5c6370
    set -g fish_color_match            56b6c2 --underline
    set -g fish_color_search_match     --background=2e6399
    set -g fish_color_operator         c678dd
    set -g fish_color_escape           56b6c2
    set -g fish_color_cwd              e06c75
    set -g fish_color_autosuggestion   5c6370
    set -g fish_color_valid_path       abb2bf --underline
    set -g fish_color_history_current  56b6c2
    set -g fish_color_selection        --background=5c6370
    set -g fish_color_user             61afef
    set -g fish_color_host             98c379
    set -g fish_color_cancel           5c6370

    # Completion pager
    set -g fish_pager_color_completion           abb2bf
    set -g fish_pager_color_prefix               c678dd
    set -g fish_pager_color_description          d1a166
    set -g fish_pager_color_progress             56b6c2
    set -g fish_pager_color_selected_background  --reverse
    set -g fish_pager_color_selected_prefix      --bold
    set -g fish_pager_color_selected_completion  --bold
    set -g fish_pager_color_selected_description --background=abb2bf

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
    # Helix-style modal editing (sshilovsky/fish-helix).  Since fish 4.3
    # this is a global (not universal) variable, so it must be set on every
    # shell start.  It MUST come before any `bind` below: assigning it
    # erases all bindings and re-runs the binding function from scratch.
    set -g fish_key_bindings fish_helix_key_bindings

    # commandline -P — Evaluates to true if the commandline is showing pager
    #                   contents, such as tab completions.

    # hjkl — navigate pager when pager is visible, otherwise self-insert
    bind -M insert h 'commandline -P; and commandline -f backward-char; or commandline -i h'
    bind -M insert j 'commandline -P; and commandline -f down-line; or commandline -i j'
    bind -M insert k 'commandline -P; and commandline -f up-line; or commandline -i k'
    bind -M insert l 'commandline -P; and commandline -f forward-char; or commandline -i l'

    # / :: slash key starts search in completion pager (like in Vim)
    bind -M insert / 'commandline -P; and commandline -f pager-toggle-search; or commandline -i /'

    # Ctrl + o :: Open yazi file manager and cd on exit
    bind -M insert ctrl-o 'yy; commandline -f repaint'

    # fish-helix
    for mode in default visual
        # G: go to last line (no count) or line N (with count) — Vim style
        bind -M $mode G 'if test "$fish_bind_count" = 0
            fish_helix_command goto_last_line
        else
            fish_helix_command goto_line
        end'
        # C-; :: alias for M-; (swap selection start/stop)
        # x   :: alias for % (select entire buffer)
        bind -M $mode 'ctrl-;' swap-selection-start-stop
        bind -M $mode x 'fish_helix_command select_all'
    end

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

    # Создание каталогов без коррекции и со всеми родительскими каталогами,
    # если они отсутствуют.
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
        alias ll="ls -lF --group-directories-first"
    end

end # status is-interactive

