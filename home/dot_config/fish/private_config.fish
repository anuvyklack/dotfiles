if status is-interactive
    # Commands to run in interactive sessions

    # Disable greeting message
    set fish_greeting

    #---------------------------------------------------------------------------
    # Color theme tuning
    #---------------------------------------------------------------------------
    set tide_character_icon 
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

    #---------------------------------------------------------------------------
    # Extra tools
    #---------------------------------------------------------------------------
    if type -q atuin;
        atuin init fish --disable-up-arrow | source
    end

    if type -q zoxide
        zoxide init fish | source
    end
    #---------------------------------------------------------------------------

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

    # / :: Start search in completion pager (like in Vim)
    bind -M insert / 'commandline -P; and commandline -f pager-toggle-search; or commandline -i /'

    # Ctrl + o :: Open yazi file manager and cd on exit
    bind -M insert \cO 'yy; commandline -f repaint'

end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/miniconda3/bin/conda
    eval /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/opt/miniconda3/etc/fish/conf.d/conda.fish"
        . "/opt/miniconda3/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/opt/miniconda3/bin" $PATH
    end
end
# <<< conda initialize <<<

