"           ██           ██   ██                ██
"          ░░           ░░   ░██               ░░
"   ██████  ██  ██████   ██ ██████     ██    ██ ██ ██████████
"  ██░░░██ ░██ ░██░░░██ ░██░░░██░     ░██   ░██░██░░██░░██░░██
" ░██  ░██ ░██ ░██  ░██ ░██  ░██      ░░██ ░██ ░██ ░██ ░██ ░██
" ░░██████ ░██ ░██  ░██ ░██  ░██       ░░████  ░██ ░██ ░██ ░██
"  ░░░░░██ ░██ ░██  ░██ ░██  ░░███  ██  ░░██   ░██ ███ ░██ ░██
"   █████  ░░  ░░   ░░  ░░    ░░░  ░░    ░░    ░░ ░░░  ░░  ░░
"  ░░░░░

echom "ginit.vim sourced"

" Nvui {{{
if exists('g:nvui')

    packadd nvui

    set guifont=Inconsolata\ LGC:h11.7
    " set guifont=FiraCode\ Nerd\ Font:h11.7
    " set guifont=InconsolataLGC\ Nerd\ Font:h11.7
    " set guifont=Liga\ Inconsolata\ LGC\ NF\ OT:h15.7


    " NvuiCmdFontFamily InconsolataLGC Nerd Font
    " NvuiCmdFontSize 25.0
    " NvuiScrollAnimationDuration 0.2

"}}}

" Neovide {{{
elseif exists('g:neovide')

    " xrandr --query | grep -A 1 connected | grep -v connected

    " xrandr | grep  " connected\|\*"
    " return the resolution of the avtive monitorc

    " xdpyinfo  | grep -oP "dimensions:\s+\K\S+"
    " returns the sum of resolutions

    " " Check the current monitor resolution if it equals to 4K.
    " if system('xdpyinfo  | grep -oP "dimensions:\s+\K\S+"') == "5120x2880\n"
    "     echom '5120x2880'
    " endif

    " lua << EOF
    " local monitor_resolution = vim.fn.system [[xdpyinfo  | grep -oP 'dimensions:\s+\K\S+']]
    " print(monitor_resolution == '5120x2880\n')
    " EOF

    set guifont=InconsolataLGC\ Nerd\ Font:h12.5

    " Remember Previous Window Size
    let neovide_remember_window_size = v:true

"}}}

" Fvim {{{
elseif exists('g:fvim_loaded')

    " good old 'set guifont' compatibility
    " set guifont=Fira\ Code:h15.4
    " set guifont=FiraCode\ NF:h15.4
    set guifont=Liga\ Inconsolata\ LGC\ NF\ OT:h15.7
    " set guifont=Liga\ Inconsolata\ LGC\ NF:h15.7

    " Smoot cursor animations
    FVimCursorSmoothMove v:true
    FVimCursorSmoothBlink v:true

    " Font tweaks
    FVimFontAntialias v:true
    FVimFontAutohint v:true
    FVimFontSubpixel v:true
    FVimFontLcdRender v:true
    FVimFontHintLevel 'full'
    " FVimFontLineHeight '+1.0' " can be 'default', '14.0', '-1.0' etc.

    " Try to snap the fonts to the pixels, reduces blur in some situations (e.g. 100% DPI).
    FVimFontAutoSnap v:true

    " Ctrl-ScrollWheel for zooming in/out
    nnoremap <silent> <C-ScrollWheelUp> :set guifont=+<CR>
    nnoremap <silent> <C-ScrollWheelDown> :set guifont=-<CR>
    nnoremap <A-CR> :FVimToggleFullScreen<CR>

"}}}

" " Neovim Qt {{{
" else " This means, we are in neovim-qt
"
"     call GuiWindowMaximized(1)
"     GuiTabline 0
"     GuiPopupmenu 0
"
"     " Use neovim-qt clipboard
"     call GuiClipboard()
"
"     " GuiFont! FiraCode NF:h12
"     " GuiFont! InconsolataLGC NF:h12
"     " GuiFont! Inconsolata LGC:h12
"     " GuiFont! Liga Inconsolata LGC NF:h12
"
"     " linespace = 3  " Расстояние между строками
"
"     " let s:fontsize = 11.5
"     " execute "guifont fira code:h" . s:fontsize
"
"     " floating font size will be supported in next version (current is 0.2.11 on 09.07.2019)
"     " guifont! liga inconsolata lgc:h11.5
"
" "}}}

endif
