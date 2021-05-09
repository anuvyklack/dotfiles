" " Make 'gf' vim keybinding work on `lua requare('module.foo')` statements.
" " For this, we need to add `.lua` extension to search name. And add `lua/`
" " directory on current level to path.
" " Taken from here:
" " https://www.reddit.com/r/vim/comments/apce2p/gf_for_lua/
"
" " Resolves 'foo' as 'foo.lua'.
" setlocal suffixesadd=.lua
"
" " Taken from :help includeexpr,
" " resolves 'modules.foo' as 'modules/foo'.
" setlocal includeexpr=substitute(v:fname,'\\.','/','g')
"
" " Now you can do 'gf' with the cursor on `modules.foo`.
"
"
" " Let's add a crude ':help include-search' support to the mix.
"
" " Tells Vim what an 'include' looks like
" setlocal include=require
"
" " Tells Vim what a 'definition' looks like
" setlocal define=function
"
" " Now you can do ':ilist <keyword>', '[<C-d>' with the cursor on <keyword>,
" " and so on.

set keywordprg=":help"
