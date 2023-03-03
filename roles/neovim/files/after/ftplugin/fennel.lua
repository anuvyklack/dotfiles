vim.bo.textwidth   = 90
vim.bo.tabstop     = 2
vim.bo.softtabstop = 2
vim.bo.shiftwidth  = 2
-- vim.bo.lisp = true
vim.bo.iskeyword = "-,!,?,#,@,48-57,_,192-255"

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
vim.bo.includeexpr = "'fnl/' .. substitute(v:fname, '\\.', '/', 'g')"

vim.bo.keywordprg = ":help"

-- Keymaps ---------------------------------------------------------------------
local Hydra = prequire('hydra')
local cmd = require('hydra.keymap-util').cmd

--   ,lv         :ConjureLogVSplit
--   ,lt         :ConjureLogTab
--   ,lq         :ConjureLogCloseVisible
-- + ,lr         :ConjureLogResetSoft
-- + ,E[motion]  :ConjureEvalMotion
-- + [Visual],E  :'<,'>ConjureEvalVisual
--   ,ee         :ConjureEvalCurrentForm
--   ,ece        :ConjureEvalCommentCurrentForm
--   ,er         :ConjureEvalRootForm
--   ,ecr        :ConjureEvalCommentRootForm
--   ,ew         :ConjureEvalWord
--   ,ecw        :ConjureEvalCommentWord
-- + ,e!         :ConjureEvalReplaceForm
-- + ,em[mark]   :ConjureEvalMarkedForm
-- + ,ef         :ConjureEvalFile
--   ,eb         :ConjureEvalBuf
-- + K           :ConjereDocWord

local hint = [[
 _w_: eval word    _cw_: word -> comment       _s_: open log in split
 _f_: eval form    _cf_: form -> comment       _v_: open log in vsplit 
 _r_: root form    _cr_: root form -> comment  _t_: open log in tab
 _b_: eval buffer  ^  ^                        _x_: reset log
 ^ ^               ^  ^                        _q_: close log
]]

Hydra({
   name = "Conjure",
   hint = hint,
   config = {
      color = 'teal',
      invoke_on_body = true,
      hint = {
         border = "rounded"
      }
   },
   body = "<leader>c",
   heads = {
      { "s",  cmd "ConjureLogSplit" },
      { "v",  cmd "ConjureLogVSplit" },
      { "t",  cmd "ConjureLogTab" },
      { "q",  cmd "ConjureLogCloseVisible" },
      { "w",  cmd "ConjureEvalWord" },
      { "cw", cmd "ConjureEvalCommentWord" },
      { "f",  cmd "ConjureEvalCurrentForm" },
      { "cf", cmd "ConjureEvalCommentRootForm" },
      { "b",  cmd "ConjureEvalBuf" },
      { "r",  cmd "ConjureEvalRootForm" },
      { "cr", cmd "ConjureEvalCommentRootForm" },
      { "x",  cmd "ConjureLogResetSoft" },
      -- { "X",  cmd "ConjureLogResetHard" },
      { '<Esc>', nil, { desc = false} }
   }
})
