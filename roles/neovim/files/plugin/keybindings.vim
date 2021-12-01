"  ██                           ██      ██              ██ ██
" ░██                          ░██     ░░              ░██░░
" ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
" ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
" ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
" ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
" ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
" ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
"                  ░░░░░                                              ░░░░░

" "              Easymotion key bindings             {{{
" " ----------------------------------------------------
" map  ; <Plug>(easymotion-prefix)
" nmap s <Plug>(easymotion-bd-f)
" xmap s <Plug>(easymotion-bd-f)
" omap s <Plug>(easymotion-bd-f)
" nmap ;w <Plug>(easymotion-w)
" nmap ;b <Plug>(easymotion-b)
" nmap ;l <Plug>(easymotion-lineanywhere)
" " -------------------------------------------------}}}

"                     Easy-Align                   {{{
" ----------------------------------------------------
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" apple   =red
" grass+=green
" sky-=   blue

" -------------------------------------------------}}}

"                  Tmux integration                {{{
" ----------------------------------------------------
let g:tmux_navigator_no_mappings = 1

" nnoremap <silent> <C-g><C-H> :TmuxNavigateLeft<cr>
" nnoremap <silent> <C-g><C-J> :TmuxNavigateDown<cr>
" nnoremap <silent> <C-g><C-K> :TmuxNavigateUp<cr>
" nnoremap <silent> <C-g><C-L> :TmuxNavigateRight<cr>

nnoremap <silent> <C-H> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-J> :TmuxNavigateDown<cr>
nnoremap <silent> <C-K> :TmuxNavigateUp<cr>
nnoremap <silent> <C-L> :TmuxNavigateRight<cr>

" nnoremap <silent> {Previous-Mapping} :TmuxNavigatePrevious<cr>
" -------------------------------------------------}}}

nnoremap <silent> gb :call ChooseBuffer()<CR>
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
endfunction
"}}}

" nnoremap <Leader>u <cmd>UndotreeToggle<CR>

" nmap <leader>vw <Plug>(wiki-index)

" Показать syntax group для участка кода, а также цвет этой группы.
" Удобно при создании своей цветовой схемы
nnoremap <silent> <C-g> <cmd>TSHighlightCapturesUnderCursor<CR>
" nnoremap <C-g> :call SyntaxAttr()<CR>

" vim: tw=76 fdm=marker
