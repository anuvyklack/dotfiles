" -------------------- Semshi ------------------------

function MyCustomHighlights()
    hi semshiGlobal      ctermfg=red guifg=#ff0000
endfunction
autocmd FileType python call MyCustomHighlights()
" Also, if you want the highlight groups to persist across
" colorscheme switches, add:
autocmd ColorScheme * call MyCustomHighlights()
