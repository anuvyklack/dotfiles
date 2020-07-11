" Inline Code In Tables: {{{
" Using single back ticks
if g:pandoc#syntax#conceal#use != 0 && index(g:pandoc#syntax#conceal#blacklist, "inlinecode") == -1
    syn region pandocNoFormattedInTables
        \ matchgroup=pandocOperator
        \ start=/\\\@<!`/ end=/\\\@<!`/
        \ nextgroup=pandocNoFormattedAttrs
        \ containedin=pandocTable,pandocGridTable,pandocPipeTable
        \ contained
        \ concealends
else
    syn region pandocNoFormattedInTables
        \ matchgroup=pandocOperator
        \ start=/\\\@<!`/ end=/\\\@<!`/
        \ nextgroup=pandocNoFormattedAttrs
        \ containedin=pandocTable,pandocGridTable,pandocPipeTable
        \ contained
endif
" }}}


hi link pandocNoFormattedInTables pandocNoFormatted
hi link pandocNoFormatted Function

if g:colors_name == 'onedark'
    hi link pandocAtxHeader PreProc
    hi link pandocSetexHeader PreProc

    hi link pandocHRule PreProc

    " Solarized Baze3
    " hi pandocCodeblock guifg=#fdf6e3

    " Solarized Baze2
    hi pandocCodeblock guifg=#eee8d5
    hi pandocCodeBlockInsideIndent guifg=#eee8d5

elseif g:colors_name == 'gruvbox'
    hi link pandocAtxHeader GruvboxOrange
    hi link pandocSetexHeader GruvboxOrange

    hi link pandocTableDelims GruvboxYellow

    hi link pandocHRule GruvboxOrange

    " hi pandocCodeblock guifg=#d7f5f9
    " hi pandocCodeblock guifg=#c5e8ed
    " hi pandocCodeblock guifg=#f2f6f7
    "
    " hi pandocCodeblock guifg=#fcd677
    hi pandocCodeblock guifg=#DDAE52

elseif g:colors_name == 'gruvbox-material'
    hi link pandocAtxHeader Tag
    hi link pandocSetexHeader Tag

    hi link pandocTableDelims Type

    hi link pandocHRule Tag

    " hi pandocCodeblock guifg=#d7f5f9
    " hi pandocCodeblock guifg=#c5e8ed
    " hi pandocCodeblock guifg=#f2f6f7
    " hi pandocCodeblock guifg=#fcd677
    hi pandocCodeblock guifg=#DDAE52

endif

" highlight link Conceal Comment
