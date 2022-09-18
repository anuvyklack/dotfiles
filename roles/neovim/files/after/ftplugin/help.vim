" Press Enter to jump to the subject (topic) under the cursor.
" Press Backspace to return from the last jump.
nnoremap <buffer> <CR> <C-]>
" nnoremap <buffer> <BS> <C-T>

setlocal textwidth=80
setlocal tabstop=8
setlocal shiftwidth=8
setlocal noexpandtab    " Не заменять табуляцию пробелами.
" setlocal nomodifiable
setlocal foldminlines=1
setlocal nospell

" foldmarker=<<<,>>>

wincmd L  " Open help in vertical split on far right.
" vertical resize 78
