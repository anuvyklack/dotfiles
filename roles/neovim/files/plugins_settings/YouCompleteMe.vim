" ----------------- YouCompleteMe --------------------

" let g:ycm_server_python_interpreter =
"             \'C:\scoop\apps\miniconda3\current\python.exe'

" Not to show preview window with documntation on complection
let g:ycm_add_preview_to_completeopt = 1

let g:ycm_autoclose_preview_window_after_completion = 0

" Close the preview window with documentation after leaving insert mode
let g:ycm_autoclose_preview_window_after_insertion = 0

let g:ycm_complete_in_comments = 0

" Complete in strings. This setting also allow file path completion
" inside strings.
let g:ycm_complete_in_strings = 1

" Use semantic compectiion on every typing
let g:ycm_semantic_triggers = {
  \   'python': [ 're!.' ]
  \ }

" Always open documentation in veertical split on the left
" autocmd BufEnter __run__,__doc__ wincmd H
