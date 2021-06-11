setlocal foldcolumn=3
" set foldnestmax=3  " deepest fold is 3 levels (only for sintax and indent)

" Use treesitter base folding.
setlocal foldmethod=expr  
setlocal foldexpr=nvim_treesitter#foldexpr()

" setlocal fillchars=fold:•
" setlocal foldtext=CppFoldText('•')

" 'space' is fold char ↓
setlocal fillchars=fold:\  " the backslash escapes a space
setlocal foldtext=CppFoldText('\ ')

" My custom fold string.
function! CppFoldText(string) "{{{

    " The tokens that will be streaped from foldtext.
    let foldtext_stop_words = [
          \ '@brief',
          \ '\\brief',
          \ ]

    " The number of indentation. All tabs count as spaces 
    " with respect to &tabstop.
    " let indent_num = indent(v:foldstart) 
    let indent_str = repeat(a:string, indent(v:foldstart))

    " The number of folded lines.
    let fold_size_num = 1 + v:foldend - v:foldstart
    let fold_size_str = " " . fold_size_num . " lines "
  
    " Remove all whitespaces from the beginning and the end of the line.
    let line = trim(getline(v:foldstart))
    
    " Take cake of Doxygen comments.
    if match(line, '^/\*\*$') != -1 " If line match to Doxygen comment line.
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
    else 
    endif
  
    " Remove stop tokens from line,
    let line = substitute(line, join(foldtext_stop_words, '\|'), '', 'g')
    let line = substitute(line, '\v\s+', ' ', 'g')

    " let line = line . '...' . close_bracket
  
    " Size of line numbers colummn 
    let nu = (&number ? len(string(line('$'))) : 0)
  
    let expansion_str = 
        \repeat(a:string, 
        \       winwidth(0) - &foldcolumn - nu - 7 -
        \       strdisplaywidth(indent_str . line . fold_size_str)
        \      )
  
    return indent_str . line . expansion_str . fold_size_str
endfunction "}}}

