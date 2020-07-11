" -------------------- Startify ----------------------

" Thw number of most recently used files
let g:startify_files_number = 10

let g:startify_bookmarks = [
\   {'m': '/mnt/d/artyu/OneDrive/Документы/Заметки/Monospace'},
\   {'t': '/mnt/c/Users/artyu/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json'},
\   {'a': '/mnt/c/Users/artyu/AppData/Roaming/alacritty/alacritty.yml'},
\]
" \   {'i': '~/.config/nvim/init.vim'},
" \   {'p': '~/.config/nvim/plugins.vim'},
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
