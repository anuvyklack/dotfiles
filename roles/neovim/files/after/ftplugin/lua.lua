vim.o.textwidth = 80

vim.o.tabstop     = 3
vim.o.softtabstop = 3
vim.o.shiftwidth  = 3

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
path:append( vim.fn.stdpath("config") .. "/lua")

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr   = "nvim_treesitter#foldexpr()"


--------------------- Keybindings ---------------------

local buf_set_keymap = require("util").buf_set_keymap
local opt = {noremap=true, silent=false}

buf_set_keymap(0, 'n', 'gK', 'Show :help', 'K', opt)
buf_set_keymap(0, 'v', 'gK', 'Show :help', 'K', opt)

-------------------------------------------------------
