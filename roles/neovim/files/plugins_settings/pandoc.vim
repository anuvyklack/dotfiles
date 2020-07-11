" --------------------- Pandoc -----------------------

let g:pandoc#filetypes#handled = ["pandoc", "markdown"]

" Enable pandoc functionality for markdown files
let g:pandoc#filetypes#pandoc_markdown = 1

" let g:pandoc#modules#enabled = ["formatting", "folding", "toc"]

let g:pandoc#modules#disabled = ["folding"]

" ----------------------------------------------------
" Folding
" ----------------------------------------------------
let g:pandoc#formatting#mode = "s"  " Use soft wraps

let g:pandoc#folding#mode = 'stacked'
let g:pandoc#folding#fdc = 0
" ----------------------------------------------------


let g:pandoc#keyboard#sections#header_style = 's'

let g:pandoc#toc#shift = 2

" ----------------- Pandoc Syntax --------------------

let g:pandoc#syntax#codeblocks#embeds#langs =
            \ ["python", "shell=sh", "bash=sh", "sh", "zsh", "json", "vim"]
