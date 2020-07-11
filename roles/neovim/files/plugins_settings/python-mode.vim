" ------------------ Python Mode ---------------------

let g:pymode_run = 1  " возможность запускать код

let g:pymode_python = 'python3'

" GoToDefinition in vertical split
let g:pymode_rope_goto_def_newwin = 'vnew'

" Always open documentation in veertical split on the left
autocmd BufEnter __run__,__doc__ wincmd H

" отключаем автокомплит по коду
" (у нас вместо него используется jedi-vim)
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0

" документация
let g:pymode_doc = 1
let g:pymode_doc_bind = 'K'  " key to show python documentation

" проверка кода
" let g:pymode_lint = 1
" let g:pymode_lint_checkers = ['pyflakes', 'pyling', 'pep8', 'mccabe']
" let g:pymode_lint_ignore="" " E501,W601,C0110
"
" let g:pymode_lint_on_write = 1  " Проверять код при сохранении
" let g:pymode_lint_on_fly = 1    " Проверять код на лету

" Show error message if cursor placed at the error line
let g:pymode_lint_message = 1

" Pylint signs
let g:pymode_lint_todo_symbol = 'WW'
let g:pymode_lint_comment_symbol = 'CC'
let g:pymode_lint_visual_symbol = 'RR'
let g:pymode_lint_error_symbol = 'EE'
let g:pymode_lint_info_symbol = 'II'
let g:pymode_lint_pyflakes_symbol = 'FF'

" let g:pymode_virtualenv = 1  " Enable virtualenvs
" let g:pymode_breakpoint_bind = '<leader>b'
" let g:pymode_virtualenv_path = $VIRTUAL_ENV  " path to the virtualenv
