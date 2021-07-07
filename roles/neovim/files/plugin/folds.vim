" The very nice expample is after link:
" https://github.com/AdamWagner/stackline/issues/42#issuecomment-696817874


set foldmethod=marker   " fold based on markers
set foldcolumn=2
" set foldlevelstart=0
" set foldnestmax=3     " deepest fold is 3 levels (only for sintax and indent)

set fillchars=fold:•
set foldtext=CustomFoldText('•')

function! CustomFoldText(string) "{{{
    " Get first non-blank line.
    let line_num = v:foldstart
    while getline(fs) =~ '^\s*$'  " if line consists only from spaces
      let line_num = nextnonblank(line_num + 1)
    endwhile

    " If fold is emty, get the first line.
    if line_num >= v:foldend
      let line_num = v:foldstart
    endif

    " The number of indentation.
    " All tabs count as spaces with respect to '&tabstop'.
    let indent_num = indent(line_num)

    " Get the line content, removing all whitespaces
    " from the beginning and the end of the line.
    let line = trim(getline(line_num))

    " Construct the list from '&commentstring' string using '%s' as separator,
    " and get the first element from the list.
    "   This list construction works the next way. The tipical '&commentstring'
    " conten looks like:
    "     commentstring=/*%s*/    (cpp example)
    " It consiste of different comment strings separeted by '%s' symbols.
    let comment_str = split(&commentstring, '%s')[0]
    " If the line starts from the first element in the obtained list (i.e. line
    " starts with the comment sign), then ...
    if match(line, '^'.comment_str) != -1
      " ... add its length to the 'indent_num' variable.
      let indent_num = indent_num + len(comment_str)
    endif

    " Remove all comment signs.
    let line = substitute(line, join(split(&commentstring, '%s'), '\|'), '', 'g')
                             "  ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                             "  In this command we construct from this string:
                             "      '/*%s*/'   (cpp example)
                             "  the next one:
                             "      '/*\|*/'
                             "  which is Vim pattern.

    " Remove all foldmarkers signs.
    let line = substitute(line, join(split(&foldmarker, ','), '\d\?\|'), '', 'g')

    let indent_num = indent_num + match(line, '\S')

    " Convert indentation level number into indentation string.
    if indent_num > 0
      let indent_str = repeat(a:string, indent_num-1) . ' '
    else
      let indent_str = ''
    endif

    " Remove all whitespaces from the beginning and the end of the line.
    " And add one at the end to separate line content from the fold signs:
    " i.e. to get this:
    "     fold text ••••••••••••••••••••••••••
    " not this:
    "     fold text•••••••••••••••••••••••••••
    let line = trim(line) . ' '

    " The number of folded lines.
    let fold_size_num = 1 + v:foldend - v:foldstart
    let fold_size_str = " " . fold_size_num . " lines "

    " Size of line numbers colummn
    let nu = (&number ? len(string(line('$'))) : 0)

    " " Try to get the value from the 'g:custom_foldtext_max_width' variable. If
    " " it doesn't exists, get the width of the current window. Substitute from
    " " this value the widths of the foldcolumn and line number column.
    " let w = get(g:, 'custom_foldtext_max_width', winwidth(0)) - &foldcolumn - nu

    " let lineCount = line("$")
    " try
    "   let foldPercentage = printf("[%.1f", (foldSize*1.0)/lineCount*100) . "%] "
    " catch /^Vim\%((\a\+)\)\=:E806/    " E806: Using Float as String
    "   let foldPercentage = printf("[of %d lines] ", lineCount)
    " endtry


    let expansion_str = repeat(a:string, winwidth(0) - &foldcolumn - nu - 7 -
                                       \ strdisplaywidth(indent_str.line.fold_size_str))

    return indent_str . line . expansion_str . fold_size_str
endfunction "}}}


" " The main part of text that will be put in fold text.
" function! FoldText(foldtext_stop_words) abort " {{{
"   return trim(substitute(
"   \ getline(v:foldstart),
"   \ '\V\C'
"   \ . join(split(&commentstring, '%s'), '\|') . '\|'
"   \ . join(split(&foldmarker, ','), '\d\?\|') . '\|'
"   \ . join(a:foldtext_stop_words, '\|') . '\|',
"   \ '',
"   \ 'g'
"   \ ))
" endfunction " }}}
"
" " let g:fold_label = ' '
" " let g:commented_label = ' '
" let g:lines_label = 'lines'
" " let g:modified_label = ' ' " alt : ' '
"
" " The tokens that will be streaped from foldtext.
" let g:foldtext_stop_words = [
"       \ '\^function',
"       \ '!',
"       \ 'abort',
"       \ ]
"
" let g:crease_foldtext = {
"       \ 'default': '%{CreaseIndent()}%{FoldTxt()} %{IsMod()} %{Cmmtd()} %= %{CountFoldText()}%l '.g:lines_label.' %f%f%f%f',
"       \}

" vim: tw=79
