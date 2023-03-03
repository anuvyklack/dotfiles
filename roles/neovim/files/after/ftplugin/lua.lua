vim.bo.textwidth   = 80
vim.bo.tabstop     = 3
vim.bo.softtabstop = 3
vim.bo.shiftwidth  = 3
-- vim.wo.foldmethod  = 'expr'

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
vim.bo.includeexpr = "'lua/' .. substitute(v:fname, '\\.', '/', 'g')"

vim.bo.keywordprg = ":help"

-- local path = vim.opt_local.path
-- path:append( vim.fn.stdpath("config") .. "/lua" )

-- Keymaps ---------------------------------------------------------------------

vim.keymap.set({'n','x'}, 'gK', 'K', { buffer = true, desc = 'Show :help' })

-- Ufo -------------------------------------------------------------------------
if pcall(require, 'ufo') then
   local make_ts_handler = require('anuvyklack.ufo.make-general-ts-handler')

   require('ufo').setFoldVirtTextHandler( vim.api.nvim_get_current_buf(),
                                          make_ts_handler() )
end

-- vim: fml=4

