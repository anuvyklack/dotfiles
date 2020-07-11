setlocal textwidth=80
setlocal conceallevel=1
setlocal concealcursor=nc
setlocal nospell
setlocal nonumber
setlocal nofoldenable
setlocal modifiable

setlocal formatoptions=tcq21jp

" replace common punctuation
iabbrev <buffer> << «
iabbrev <buffer> >> »

nnoremap <silent> <leader>t :TOC<CR>
