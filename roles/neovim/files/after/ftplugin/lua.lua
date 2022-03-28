vim.bo.textwidth   = 80
vim.bo.tabstop     = 3
vim.bo.softtabstop = 3
vim.bo.shiftwidth  = 3
vim.wo.foldmethod  = 'expr'

local pretty_fold_available, pretty_fold = pcall(require, 'pretty-fold')
if pretty_fold_available then
   pretty_fold.ft_setup('lua', {
      process_comment_signs = false ,
      matchup_patterns = {
         { '^%s*do$', 'end' }, -- `do ... end` blocks
         { '^%s*if', 'end' },  -- if
         { '^%s*for%s', 'end' }, -- for
         { 'function%s*%(', 'end' }, -- 'function( or 'function (''
         { '{', '}' },
         { '%(', ')' }, -- % to escape lua pattern char
         { '%[', ']' }, -- % to escape lua pattern char
      }
   })
end

-- Keybindings -----------------------------------------------------------------

-- require('util').keymap.set({'n','v'}, 'gK', 'K', {desc = 'Show :help', buffer = true})
vim.keymap.set({'n','v'}, 'gK', 'K', {desc = 'Show :help', buffer = true})

--------------------------------------------------------------------------------

-- Make 'gf' vim keybinding work on `lua requare('module.foo')` statements.
-- For this, we need to add `.lua` extension to search name. And add `lua/`
-- directory in '~/.config/nvim/lua' to path.
-- Taken from here:
-- https://www.reddit.com/r/vim/comments/apce2p/gf_for_lua/

-- Set in Neovim by default
-- vim.bo.suffixesadd = ".lua"  -- Resolves 'foo' as 'foo.lua'.

-- Taken from :help includeexpr
-- Substitute '.' with '/' to resolve 'modules.foo' as 'modules/foo'.
vim.bo.includeexpr = "substitute(v:fname, '\\.', '/', 'g')"

vim.bo.keywordprg = ":help"

-- The same as 'setlocal path+='
local path = vim.opt_local.path
path:append( vim.fn.stdpath("config") .. "/lua" )

--------------------------------------------------------------------------------
