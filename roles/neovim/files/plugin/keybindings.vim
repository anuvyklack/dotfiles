"  ██                           ██      ██              ██ ██
" ░██                          ░██     ░░              ░██░░
" ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
" ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
" ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
" ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
" ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
" ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
"                  ░░░░░                                              ░░░░░

"                       Tmux integration                       {{{
" ----------------------------------------------------------------
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <C-H> <Cmd>TmuxNavigateLeft<CR>
nnoremap <silent> <C-J> <Cmd>TmuxNavigateDown<CR>
nnoremap <silent> <C-K> <Cmd>TmuxNavigateUp<CR>
nnoremap <silent> <C-L> <Cmd>TmuxNavigateRight<CR>

" nnoremap <silent> {Previous-Mapping} <Cmd>TmuxNavigatePrevious<CR>
" -------------------------------------------------------------}}}

"                         Vim-Smartword                        {{{
" ----------------------------------------------------------------
" Make vim treat all word delimiters like it treats spaces (for word motions).
map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge <Plug>(smartword-ge)

noremap ,w  w
noremap ,b  b
noremap ,e  e
noremap ,ge ge
" -------------------------------------------------------------}}}

nnoremap <silent> gb <Cmd>call ChooseBuffer()<CR>
function! ChooseBuffer()  "{{{
    " Количество открытых буферов
    let num_of_buffers = len(getbufinfo({'buflisted':1}))
    if num_of_buffers > 2
        " If you are interesting what is <C-z> check ':help wildcharm'.
        " call feedkeys(":buffer \<C-z>")
        ToggleBufExplorer
    else
        bnext
    endif
endfunction "}}}

" nnoremap <Leader>u <Cmd>UndotreeToggle<CR>

" nmap <leader>vw <Plug>(wiki-index)

" Показать syntax group для участка кода, а также цвет этой группы.
" Удобно при создании своей цветовой схемы
nnoremap <silent> <C-g> <Cmd>TSHighlightCapturesUnderCursor<CR>
" nnoremap <C-g> <cmd>call SyntaxAttr()<CR>

" vim: tw=76 fdm=marker
