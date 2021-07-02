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
