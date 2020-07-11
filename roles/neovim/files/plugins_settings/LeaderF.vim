" Show hidden file and directories
let g:Lf_ShowHidden = 1
let g:Lf_UseCache = 0

" Show LeaderF window in popup or floating window
let g:Lf_WindowPosition = 'popup'
let g:Lf_WindowHeight = 0.7
let g:Lf_ShowRelativePath = 0
let g:Lf_CursorBlink = 0

" Follow the symlinks
let g:Lf_FollowLinks = 1

" Show the preview of the code the tag locates in when navigating the tags.
let g:Lf_PreviewCode = 1

" The search string you typed during last search is
" still there when LeaderF is launched again.
let g:Lf_RememberLastSearch = 1

" Show files in submosules of Git repo
let g:Lf_RecurseSubmodules = 0

" Remove the current buffer name from the result list.
let g:Lf_IgnoreCurrentBufferName = 1

let g:Lf_PopupHeight = float2nr(&lines * 0.7)
let g:Lf_WorkingDirectoryMode = 'Ac'


" Specify the files and directories you want to exclude while indexing.
let g:Lf_WildIgnore = {
        \ 'dir': ['.git'],
        \ 'file': ['*.exe', '*.pdf']
        \}

let g:Lf_PreviewResult = {
        \ 'File': 0,
        \ 'Buffer': 1,
        \ 'Mru': 0,
        \ 'Tag': 0,
        \ 'BufTag': 1,
        \ 'Function': 1,
        \ 'Line': 1,
        \ 'Colorscheme': 0,
        \ 'Rg': 0,
        \ 'Gtags': 0
        \}

" Swttig the mappings in normal mode.
let g:Lf_NormalMap = {
    \ "File":   [["<ESC>", ':exec g:Lf_py "fileExplManager.quit()"<CR>'],
    \            ["<F6>", ':exec g:Lf_py "fileExplManager.quit()"<CR>']
    \           ],
    \ "Buffer": [["<ESC>", ':exec g:Lf_py "bufExplManager.quit()"<CR>'],
    \            ["<F6>", ':exec g:Lf_py "bufExplManager.quit()"<CR>']
    \           ],
    \ "Mru":    [["<ESC>", ':exec g:Lf_py "mruExplManager.quit()"<CR>']],
    \ "Tag":    [],
    \ "BufTag": [],
    \ "Function": [],
    \ "Line":   [],
    \ "History":[],
    \ "Help":   [],
    \ "Self":   [],
    \ "Colorscheme": []
    \}

" " Specify a list of ripgrep configurations. For example, >
" let g:Lf_RgConfig = [
"     \ "--max-columns=150",
"     \ "--type-add web:*.{html,css,js}*",
"     \ "--glob=!git/*",
"     \ "--hidden"
" \ ]

" Configure the colorscheme of statusline for LeaderF
" The colorscheme files can be found in the directory:
" LeaderF/autoload/leaderf/colorscheme/
" let g:Lf_StlColorscheme = 'gruvbox_material'
let g:Lf_PopupColorscheme = 'gruvbox_material'
" let g:Lf_PopupColorscheme = 'default'
