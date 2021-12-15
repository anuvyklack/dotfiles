" Press Enter to jump to the subject (topic) under the cursor.
" Press Backspace to return from the last jump.
nnoremap <buffer> <CR> <C-]>
nnoremap <buffer> <BS> <C-T>

setlocal textwidth=80
setlocal tabstop=8
setlocal shiftwidth=8
setlocal noexpandtab    " Не заменять табуляцию пробелами.
setlocal nomodifiable
setlocal foldminlines=1
setlocal nospell

" foldmarker=<<<,>>>

" setlocal bufhidden=unload
                " We don't need this option, because if you open help buffer
                " using ':help' command the special 'buftype=help' option is
                " using (see :help special-buffers). And when you open file
                " with 'filetype=help' as usual buffer to edit it's content
                " the 'bufhidden=unload' option drop all unsaved changes when
                " the buffer become hidden (no more open in any window).

" wincmd L  " Open help in vertical split on topleft
" vertical resize 78
