-- nvim --clean -u mini.lua mini.lua

vim.o.packpath = '~/.local/share/nvim/site'

vim.cmd 'packadd yanky'
require('yanky').setup({
  system_clipboard = {
    sync_with_ring = false,
  },
})
