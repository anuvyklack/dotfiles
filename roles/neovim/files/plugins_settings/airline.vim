" --------------------- Airline ----------------------

" активировать powerline символы
let g:airline_powerline_fonts = 1
let g:Powerline_symbols='unicode'  " Поддержка unicode

let g:airline#extensions#keymap#enabled = 0  " show using keymap

" Список открытых буферов; help airline-tabline
let g:airline#extensions#tabline#enabled = 1

let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_tab_count = 1
let g:airline#extensions#tabline#show_tab_type = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

let g:airline_left_sep=''
let g:airline_right_sep=''

let g:airline_left_alt_sep=''
let g:airline_right_alt_sep=''

let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '│'
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#right_alt_sep = '│'

" Кастомная секция с расположением курсора относительно файла
let g:airline_section_z = " %l/%L \ue0a1: %c"

" whether close button in tabline should be shown
let g:airline#extensions#tabline#show_close_button = 0

" wordcount

let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#wordcount#filetypes =
    \ ['asciidoc', 'help', 'mail', 'markdown', 'org', 'rst', 'tex', 'text', 'pandoc']

" Ale extension
let g:airline#extensions#ale#enabled = 1
