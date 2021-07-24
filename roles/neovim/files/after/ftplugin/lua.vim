" " Make 'gf' vim keybinding work on `lua requare('module.foo')` statements.
" " For this, we need to add `.lua` extension to search name. And add `lua/`
" " directory in '~/.config/nvim/lua' to path.
" " Taken from here:
" " https://www.reddit.com/r/vim/comments/apce2p/gf_for_lua/
"
" " Set in Neovim by default
" " setlocal suffixesadd=.lua " Resolves 'foo' as 'foo.lua'.
"
" " Substitute '.' with '/' to resolve 'modules.foo' as 'modules/foo'.
" " See :help includeexpr.
" setlocal includeexpr=substitute(v:fname,'\\.','/','g')
"
" exec "setlocal path+=" . stdpath("config") . "/lua"
"
" setlocal keywordprg=":help"
"
" " Use treesitter base folding.
" setlocal foldmethod=expr
" setlocal foldexpr=nvim_treesitter#foldexpr()


"-----------------------------------------------------------------------------
" Folds
"-----------------------------------------------------------------------------
set fillchars+=fold:•
set foldtext=CustomFoldText('•')

function! CustomFoldText(string) "{{{

    " The tokens that will be streaped from foldtext.
    let foldtext_stop_words = []

    " Get first non-blank line.
    let line_num = v:foldstart
    while getline(fs) =~ '^\s*$'  " if line consists only from spaces
      let line_num = nextnonblank(line_num + 1)
    endwhile

    " If fold is emty, get the first line.
    if line_num >= v:foldend
      let line_num = v:foldstart
    endif

    " Get the number of indentations.
    " All tabs count as spaces with respect to '&tabstop' option.
    let indent_num = indent(line_num)

    " Get the line content, removing all whitespaces
    " from the beginning and the end of the line.
    let line = trim(getline(line_num))

    " Construct the list from '&commentstring' string using '%s' as separator,
    " and get the first element from the list.
    "   This list construction works the next way.
    " The tipical '&commentstring' conten looks like:
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
    "                           ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    "                           In this command we construct from this string:
    "                               '/*%s*/'   (cpp example)
    "                           the next one:
    "                               '/*\|*/'
    "                           which is Vim pattern.

    " Remove all foldmarkers signs.
    let line = substitute(line, join(split(&foldmarker, ','), '\d\?\|'), '', 'g')

    let indent_num = indent_num + match(line, '\S')

    " Convert indentation level number into indentation string.
    if indent_num > 0
      let indent_str = repeat(a:string, indent_num - 1) . ' '
    else
      let indent_str = ''
    endif

    let last_line = trim(getline(v:foldend))
    if (match(last_line, '^}') != -1)
        let last_line = substitute(last_line, '[,;].*$', '', '')
        let line = line . '...' . last_line
    endif

    " Remove stop tokens from line,
    let line = substitute(line, join(foldtext_stop_words, '\|'), '', 'g')
    let line = substitute(line, '\v\s+', ' ', 'g')

    let line = trim(line)

    " And add one at space the end to separate
    " line content from the fold signs:
    " i.e. to get this:
    "     fold text •••••
    " but not this:
    "     fold text••••••
    let line = line . ' '

    " The number of folded lines.
    let fold_size_num = 1 + v:foldend - v:foldstart
    let fold_size_str = " " . fold_size_num . " lines "

    " Size of line numbers colummn
    let nu = (&number ? len(string(line('$'))) : 0)

    let expansion_str =
    \    repeat(a:string,
    \           winwidth(0) - &foldcolumn - nu - 10 -
    \           strdisplaywidth(indent_str . line . fold_size_str)
    \          )

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
