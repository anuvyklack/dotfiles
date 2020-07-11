" -------------------- NERD Tree ---------------------

" Показывать скрытые файлы по умолчанию
let NERDTreeShowHidden = 0

" Automatically close NerdTree when you open a file
let NERDTreeQuitOnOpen = 0

" Automatically delete the buffer of the file you just deleted with NerdTree
let NERDTreeAutoDeleteBuffer = 1

" disable “Press ? for help”
let NERDTreeMinimalUI = 0

let g:NERDTreeHijackNetrw = 1

let g:NERDTreeDirArrowExpandable  = "▷"
let g:NERDTreeDirArrowCollapsible = "◢"

" Close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
