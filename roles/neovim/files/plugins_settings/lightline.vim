set noshowmode     " Disable show mode info
set showtabline=2  " Always show tabline

let g:lightline = {}

" Separators {{{

let g:lightline.separator    = { 'left': '', 'right': '' } " left: '\ue0b8', right: '\ue0be'
let g:lightline.subseparator = { 'left': '', 'right': '' } " left: '\ue0b9', right: '\ue0b9'

let g:lightline.tabline_separator    = { 'left': '', 'right': '' } " left: '\ue0bc', right: '\ue0ba'
let g:lightline.tabline_subseparator = { 'left': '', 'right': '' } " left: '\ue0bb', right: '\ue0bb'

" let g:lightline.separator    = { 'left': '', 'right': '' }  " '\ue0cc'
" let g:lightline.subseparator = { 'left': '', 'right': '' }  " '\ue0cd'

" let g:lightline.separator = { 'left': '', 'right': '' }  " left: '\ue0d2', right: '\ue0d4'

" let g:lightline.separator    = { 'left': '', 'right': '' } " left: '\ue0bc', right: '\ue0ba'
" let g:lightline.subseparator = { 'left': '', 'right': '' } " left: '\ue0bb', right: '\ue0bb'

" let g:lightline.tabline_separator    = { 'left': '', 'right': '' } " left: '\ue0b8', right: '\ue0be'
" let g:lightline.tabline_subseparator = { 'left': '', 'right': '' } " left: '\ue0b9', right: '\ue0b9'

" }}}

let g:lightline#ale#indicator_checking = ''  " '\uf110'
let g:lightline#ale#indicator_warnings = ''  " '\uf529'
let g:lightline#ale#indicator_errors   = ''  " '\uf00d'
let g:lightline#ale#indicator_ok       = ''  " '\uf00c'

let g:lightline_gitdiff#indicator_added    = '+'
let g:lightline_gitdiff#indicator_deleted  = '-'
let g:lightline_gitdiff#indicator_modified = '*'
let g:lightline_gitdiff#min_winwidth = '70'

let g:lightline#asyncrun#indicator_none = ''
let g:lightline#asyncrun#indicator_run = 'Running...'

" https://github.com/itchyny/lightline.vim/issues/279

" lightline.active {{{
let g:lightline.active = {
    \ 'left': [ [ 'servicewindow', 'mode', 'paste' ],
    \           [ 'readonly', 'filename', 'modified' ]],
    \ 'right': [ [ 'lineinfo', 'keymap' ],
    \            [ 'devicons_fileformat', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok'],
    \            [ 'asyncrun_status', 'coc_status', 'devicons_filetype' ] ]
    \ }
    " [ 'devicons_fileformat', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok', 'pomodoro'],
" }}}

" lightline.inactive {{{
let g:lightline.inactive = {
    \ 'left': [ [ 'servicewindow', 'readonly-inactive', 'filename', 'modified' ] ],
    \ 'right': [ [ 'devicons_filetype', 'devicons_fileformat' ] ]
    \ }
" }}}

" lightline.tabline {{{
let g:lightline.tabline = {
    \ 'left': [ [ 'vim_logo', 'tabs' ] ],
    \ 'right': [ [ 'gitbranch' ],
    \            [ 'gitstatus' ] ]
    \ }
" }}}

" lightline.tab {{{
let g:lightline.tab = {
    \ 'active': [ 'activetabnum', 'artify_filename', 'modified' ],
    \ 'inactive': [ 'artify_inactivetabnum', 'filename', 'modified' ] }
" }}}

" lightline.component {{{
let g:lightline.component = {
    \ 'absolutepath': '%F',
    \ 'bufinfo': '%{bufname("%")}:%{bufnr("%")}',
    \ 'bufnum': '%n',
    \ 'charvalue': '%b',
    \ 'charvaluehex': '%B',
    \ 'close': '%999X X ',
    \ 'filesize': "%{HumanSize(line2byte('$') + len(getline('$')))}",
    \ 'paste': '%{&paste ? "PASTE" : ""}',
    \ 'spell': '%{ &spell ? &spelllang : ""}',
    \ 'relativepath': '%f',
    \ 'vim_logo': "\ue7c5",
    \ }
    " \ 'lineinfo': ' %2l/%L : %-2v',
    " \ 'lineinfo-original': '%2p%% %2l/%-3L: %2v',
" }}}

" lightline.component_function {{{
let g:lightline.component_function = {
    \ 'coc_currentfunction': 'CocCurrentFunction',
    \ 'coc_status': 'coc#status',
    \ 'devicons_filetype': 'Devicons_Filetype',
    \ 'devicons_fileformat': 'Devicons_Fileformat',
    \ 'filename': 'Lightline_Filename',
    \ 'gitbranch' : 'Gitbranch',
    \ 'gitstatus' : 'lightline_gitdiff#get_status',
    \ 'lineinfo': 'Lightline_Lineinfo',
    \ 'mode': 'Lightline_Mode',
    \ 'modified': 'Lightline_Modified',
    \ 'pomodoro': 'PomodoroStatus',
    \ 'pomodoro2': 'pomo#status_bar',
    \ 'readonly-inactive': 'Lightline_Readonly',
    \ 'servicewindow': 'Lightline_Service_Window',
    \ 'winnr': 'winnr'
    \ }


" }}}

" lightline.component_expand {{{
let g:lightline.component_expand = {
    \ 'linter_checking': 'lightline#ale#checking',
    \ 'linter_warnings': 'lightline#ale#warnings',
    \ 'linter_errors': 'lightline#ale#errors',
    \ 'linter_ok': 'lightline#ale#ok',
    \ 'asyncrun_status': 'lightline#asyncrun#status',
    \ 'readonly': 'Lightline_Readonly',
    \ 'keymap': 'Lightline_Keymap'
    \ }
" }}}

" lightline.tab_component {{{
let g:lightline.tab_component = {
    \ }
" }}}

" lightline.tab_component_function {{{
let g:lightline.tab_component_function = {
    \ 'activetabnum': 'Active_tab_num',
    \ 'artify_inactivetabnum': 'Inactive_tab_num',
    \ 'artify_filename': 'Lightline_tab_filename',
    \ 'filename': 'lightline#tab#filename',
    \ 'modified': 'lightline#tab#modified',
    \ 'readonly': 'lightline#tab#readonly',
    \ 'tabnum': 'lightline#tab#tabnum'
    \ }
" }}}

" lightline.component_type {{{
let g:lightline.component_type = {
    \ 'linter_warnings': 'warning',
    \ 'linter_errors': 'error',
    \ 'readonly': 'error',
    \ 'keymap': 'language',
    \ }
" }}}

" lightline.component_visible_condition {{{
let g:lightline.component_visible_condition = {
    \ 'gitstatus': 'lightline_gitdiff#get_status() !=# ""'
    \ }
" }}}

augroup lightlineCustom
    au!
    au BufWritePost * call lightline_gitdiff#query_git() | call lightline#update()
    au OptionSet iminsert call lightline#update()
    au TextChanged,TextChangedI,TextChangedP * call lightline#update()
augroup END

fu! WindowType()  "{{{
    " Return the type of the window if it is a service window,
    " else return empty.
    if exists('b:windowtype') | return b:windowtype | endif

    let fname = expand('%:t')

    if     fname =~ '__Tagbar__'     | let wt = 'Tagbar'
    elseif fname == '[Command Line]' | let wt = 'Command Line'
    elseif fname =~ 'diffpanel'      | let wt = 'Undo Diff'
    elseif &ft   == 'bufexplorer'    | let wt = 'BufExplorer'
    elseif &ft   == 'GV'             | let wt = 'Git Commit Browser'
    elseif &ft   == 'Git'            | let wt = 'Git'
    elseif &ft   == 'help'           | let wt = 'HELP'
    elseif &ft   == 'man'            | let wt = 'MAN'
    elseif &ft   == 'nerdtree'       | let wt = 'NERD Tree'
    elseif &buftype == 'quickfix'    | let wt = 'Quickfix'
    elseif &ft   == 'undotree'       | let wt = 'Undo Tree'
    elseif &ft   == 'vim-plug'       | let wt = 'Vim Plug'
    else
        let wt = ''
    endif

    let b:windowtype = wt
    return b:windowtype

endf  "}}}
fu! WindowTypeForExpandedComponents()  "{{{
    " Костыль, потому что нормальныя WindowType() не работает с
    " expanded_cmponents: возвращает пустую строку для всех окон кроме
    " command line.

    let fname = expand('%:t')

    if     fname == '__Tagbar__'     | let wt = 'Tagbar'
    elseif fname == '[Command Line]' | let wt = 'Command Line'
    elseif fname =~ 'diffpanel'      | let wt = 'Undo Diff'
    elseif &ft   == 'bufexplorer'    | let wt = 'BufExplorer'
    elseif &ft   == 'help'           | let wt = 'HELP'
    elseif &ft   == 'man'            | let wt = 'MAN'
    elseif &ft   == 'nerdtree'       | let wt = 'NERD Tree'
    elseif &ft   == 'undotree'       | let wt = 'Undo Tree'
    else
        let wt = ''
    endif

    return wt

endf  "}}}
fu! HelpWindow()  "{{{
    " Check if current window is a help window.
    let helptypes = ['help', 'man', 'GV', 'git']

    if index(helptypes, &ft) >= 0
        return 1
    else
        return 0
    endif

endf  "}}}

fu! Lightline_Filename() "{{{

    " if exists('w:statusline_pre')
    "     return w:statusline_pre
    " endif

    if &buftype == 'quickfix'
        return get(w:, 'quickfix_title')
    elseif WindowType() == '' || HelpWindow()
        return expand('%:t')
    else
        return ''
    endif

endf  "}}}
fu! Lightline_Keymap()  "{{{
    " return &iminsert == 0 ? 'EN' : 'RU'
    if &iminsert == 0
        " return 'En'
        return ''
    elseif &iminsert == 1
        " return b:keymap_name  " return currently keymap name
        return 'Ru'
    endif
endf  "}}}
fu! Lightline_Lineinfo()  "{{{

    if WindowType() == '' || HelpWindow()
        return printf(' %2d:%d : %-2d', line('.'), line('$'), col('.'))
    else
        return ''
    endif

endf  "}}}
fu! Lightline_Mode() "{{{

    if WindowType() == ''  &&  winwidth(0) > 60
        if &showmode == 1 | let &showmode = 0 | endif
        return lightline#mode()
    else
        let &showmode = 1
        return ''
    endif

endf "}}}
fu! Lightline_Modified()  "{{{

    " return &ft =~ 'help\|vimfiler' ? '' : &modified ? '+' : &modifiable ? '' : '-'

    if WindowType() == '' || HelpWindow()
        return &modified ? '+' : &modifiable ? '' : '-'
    else
        return ''
    endif

endf  "}}}
fu! Lightline_Service_Window()  "{{{

    let wt = WindowType()
    return (wt != '' ? wt : '')

endf  "}}}
fu! Lightline_Readonly()  "{{{
    " '\ue0a2' = 

    if (WindowTypeForExpandedComponents()=='' || HelpWindow())  &&  &readonly
        return  "\ue0a2"
    else
        return ''
    endif

endf  "}}}
fu! Devicons_Filetype() "{{{

    if WindowType() != '' | return '' | endif

    " return winwidth(0) > 70 ? (strlen(&filetype) ? WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''

endf  "}}}
fu! Devicons_Fileformat() " {{{

    " return ''

    if WindowType() != '' | return '' | endif

    " return winwidth(0) > 70 ? (&fenc !=# "" ? &fenc .' '. WebDevIconsGetFileFormatSymbol() : &enc) : ''
    return winwidth(0) > 70 ? ' '.(&fenc !=# "" ? &fenc .' '. WebDevIconsGetFileFormatSymbol() : &enc).' ' : ''

endf  " }}}
fu! Inactive_tab_num(n) abort " {{{
    return a:n ." \ue0bb"
endf  "}}}
fu! Lightline_tab_filename(s) abort " {{{
    return lightline#tab#filename(a:s)
endf  "}}}
fu! CocCurrentFunction() " {{{
    return get(b:, 'coc_current_function', '')
endf  "}}}
fu! Gitbranch() abort " {{{
    if gitbranch#name() !=# ''
        return gitbranch#name() ." \ue725"
    else
        return "\ue61b"
    endif
endf " }}}
fu! PomodoroStatus() abort  "{{{

    if WindowType() != '' | return '' | endif

    if pomo#remaining_time() ==# '0'
        return "\ue001"
    else
        return "\ue003 ".pomo#remaining_time()
    endif

endf  "}}}

fu! Active_tab_num(n) abort " {{{
    return a:n ." \ue0bb"
endf  "}}}
fu! SwitchLightlineColorScheme(color) "{{{
    let g:lightline.colorscheme = a:color
    call lightline#init()
    call lightline#colorscheme()
    call lightline#update()
endf   "}}}
fu! Line_num() abort " {{{
    return string(line('.'))
endf  "}}}
fu! Line_percent() abort " {{{
    return string((100*line('.'))/line('$'))
endf  "}}}
fu! Col_num() abort " {{{
    return string(getcurpos()[2])
endf  "}}}

" }}}
