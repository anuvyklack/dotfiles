" https://github.com/simrat39/symbols-outline.nvim/issues/7
autocmd FileType Outline call SymbolsOutlineSettings()

function! SymbolsOutlineSettings()
    " setlocal foldlevel=1
    setlocal foldcolumn=0
    setlocal foldmethod=expr
    setlocal foldexpr=FoldOutline(v:lnum)
endfunction

function! FoldOutline(lnum) "{{{
    let l:line = getline(v:lnum)
    let l:nextline = getline(v:lnum + 1)

    let l:linematch = matchstrpos(l:line,  '.*[├└]')
    let l:nextlinematch = matchstrpos(l:nextline,  '.*[├└]')

    let l:linelevel = 0
    if l:linematch[2] >= 0
        let l:linelevel = max([1, (l:linematch[2] - 2)/4 + 1])
    endif

    let l:nextlinelevel = 0
    if l:nextlinematch[2] >= 0
        let l:nextlinelevel = max([1, (l:nextlinematch[2] - 2)/4 + 1])
    endif

    if l:nextlinelevel > l:linelevel
        return l:nextlinelevel
    endif

    return l:linelevel
endfunction "}}}

" vim: fdm=marker
