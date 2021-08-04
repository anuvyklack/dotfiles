set textwidth=80
set colorcolumn=+1      " Показывать ограничительную линию в следующей колонке
                        " после textwidth.

"-----------------------------------------------------------------------------
" Folds
"-----------------------------------------------------------------------------
setlocal foldcolumn=3
" set foldnestmax=3  " deepest fold is 3 levels (only for sintax and indent)

" Use treesitter base folding.
setlocal foldmethod=expr
setlocal foldexpr=nvim_treesitter#foldexpr()

setlocal fillchars+=fold:•
setlocal foldtext=CppFoldText('•')

" " use 'space' as fold char
" setlocal fillchars+=fold:\  " the backslash escapes the space char
" setlocal foldtext=CppFoldText('\ ')

" My custom fold string.
function! CppFoldText(string) "{{{

    " The tokens that will be streaped from foldtext.
    let foldtext_stop_words = [
        \   '@brief',
        \   '\\brief',
        \ ]

    " " Get the number of indentations of the first line of fold text and make
    " " the string consists of fill characters with length equal to this number.
    " let indent_str = repeat(a:string, indent(v:foldstart))

    " Get the number of indentations of the first line of fold text.
    " All tabs count as spaces with respect to '&tabstop' option.
    let indent_num = indent(v:foldstart)

    " Convert indentation level number into indentation string.
    if indent_num > 0
      let indent_str = repeat(a:string, indent_num - 1) . ' '
    else
      let indent_str = ''
    endif

    " The number of folded lines.
    let fold_size_num = 1 + v:foldend - v:foldstart
    let fold_size_str = " " . fold_size_num . " lines "

    " Remove all whitespaces from the beginning and the end of the line.
    let line = trim(getline(v:foldstart))

    " Take cake of Doxygen comments.
    if match(line, '^/\*\*$') != -1 " If line match to Doxygen comment line.
    "                 ^^---------------
    "                 escaping asterisk
        let second_line = trim(getline(v:foldstart + 1))
        let second_line = substitute(second_line, '^\*\s*', '', '')
        let line = join([line, second_line])
    endif

    let last_line = trim(getline(v:foldend))

    " if match(line, '^{$') != -1 && match(last_line, '^}') != -1
    " if ((match(line, '^{$') != -1) && (match(last_line, '^}') != -1))
    " if (match(last_line, '^};\?$') != -1)
    " if (match(last_line, '^}\.*;\?') != -1)
    if (match(last_line, '^}') != -1)
        " let last_line = substitute(last_line, '//.*$', '', '')
        let last_line = substitute(last_line, ';.*$', '', '')
        let line = line . '...' . last_line
    endif

    " Remove stop tokens from line,
    let line = substitute(line, join(foldtext_stop_words, '\|'), '', 'g')
    let line = substitute(line, '\v\s+', ' ', 'g')

    " And add one at space the end to separate
    " line content from the fold signs:
    " i.e. to get this:
    "     fold text •••••
    " but not this:
    "     fold text••••••
    let line = line . ' '

    " Size of line numbers colummn
    let nu = (&number ? len(string(line('$'))) : 0)

    let expansion_str =
    \    repeat(a:string,
    \           winwidth(0) - &foldcolumn - nu - 7 -
    \           strdisplaywidth(indent_str . line . fold_size_str)
    \          )

    return indent_str . line . expansion_str . fold_size_str
endfunction "}}}
