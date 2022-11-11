local ufo = require('ufo')

vim.o.foldcolumn = '1'
vim.o.foldlevel = 99
vim.o.foldlevelstart = -1
vim.o.foldminlines = 2

vim.keymap.set('n', 'zR', ufo.openAllFolds)
vim.keymap.set('n', 'zM', ufo.closeAllFolds)
--------------------------------------------------------------------------------

local ft_map = {
   lua = { 'treesitter', 'indent' }
    -- vim = 'indent',
    -- python = {'indent'},
    -- git = ''
}

--  
ufo.setup {
   enable_get_fold_virt_text = true,
   -- fold_virt_text_handler = handler,
   provider_selector = function(bufnr, filetype)
      return ft_map[filetype] or { 'treesitter', 'indent' }
   end
}

-- https://github.com/nvim-telescope/telescope.nvim/issues/559
--------------------------------------------------------------------------------
-- BufWinEnter is needed to take into account modeline settings.  And why it is
-- not used alone but after BufRead, is due to need to execute BufWinEnter
-- autocommand only once for each buffer.
--------------------------------------------------------------------------------
-- vim.cmd 'autocmd BufRead * autocmd BufWinEnter * ++once normal zxzM'
vim.api.nvim_create_autocmd('BufRead', {
   callback = function()
      vim.api.nvim_create_autocmd('BufWinEnter', {
         once = true,
         callback = function()
            vim.defer_fn(function()
               -- vim.api.nvim_feedkeys('zM', 'm', false)
               vim.cmd 'normal zM'
            end, 70)
         end
      })
   end
})

