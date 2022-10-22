---
--- Mini-config file for debugging.  To use run from shell:
--- nvim --clean -u mini.lua mini.lua
---
vim.o.packpath = '~/.local/share/nvim/site'

-- vim.cmd 'packadd yanky'
-- require('yanky').setup({
--   system_clipboard = {
--     sync_with_ring = false,
--   },
-- })

vim.cmd 'packadd telescope'
require('telescope')
