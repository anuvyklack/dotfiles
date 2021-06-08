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
    " The number of indentation. All tabs count as spaces 
    " with respect to &tabstop.
    " let indent_num = indent(v:foldstart) 
    let indent_str = repeat(a:string, indent(v:foldstart))
  
    " The number of folded lines.
    let fold_size_num = 1 + v:foldend - v:foldstart
    let fold_size_str = " " . fold_size_num . " lines "
  
    " Remove all whitespaces from the beginning and the end of the line.
    let line = trim(getline(v:foldstart))
    
    if match(line, '^{$') != -1
        let close_bracket = '}'
    else 
        let close_bracket = ''
    endif
  
    let line = line . '...' . close_bracket
  
    " Size of line numbers colummn 
    let nu = (&number ? len(string(line('$'))) : 0)
  
    let expansion_str = 
        \repeat(a:string, 
        \       winwidth(0) - &foldcolumn - nu - 7 -
        \       strdisplaywidth(indent_str . line . fold_size_str)
        \      )
  
    return indent_str . line . expansion_str . fold_size_str
endfunction "}}}

