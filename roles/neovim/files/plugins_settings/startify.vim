" -------------------- Startify ----------------------

" Thw number of most recently used files
let g:startify_files_number = 10

let g:startify_bookmarks = [
\   {'i': '~/.config/nvim/init.vim'},
\   {'p': '~/.config/nvim/plugins.vim'},
\]
" '~/.zshrc'

let g:startify_lists = [
\   { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
\   { 'type': 'files',     'header': ['   MRU']            },
\   { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
\   { 'type': 'sessions',  'header': ['   Sessions']       },
\   { 'type': 'commands',  'header': ['   Commands']       },
\ ]

let g:startify_fortune_use_unicode = 1

" autocmd! TabNewEntered * Startify
