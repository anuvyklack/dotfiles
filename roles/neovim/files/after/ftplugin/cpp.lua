vim.bo.textwidth   = 100
vim.bo.tabstop     = 4
vim.bo.softtabstop = 4
vim.bo.shiftwidth  = 4

vim.bo.commentstring = '// %s'

vim.bo.cindent = true -- stricter rules for C programs
vim.opt.cinoptions:append('g0')

-- Make ------------------------------------------------------------------------
vim.bo.makeprg = vim.fn.stdpath('config')..'/assets/bin/vim-cmake-makeprg'

vim.g.compiler_gcc_ignore_unmatched_lines = 1

-- Ufo -------------------------------------------------------------------------
if pcall(require, 'ufo') then
   local make_ts_handler = require('anuvyklack.ufo.make-general-ts-handler')
   local ts_handler = make_ts_handler()
   local bufnr = vim.api.nvim_get_current_buf()
   require('ufo').setFoldVirtTextHandler(bufnr, ts_handler)
end

-- vim: fml=4

