vim.keymap.set('n', '<BS>', '<Plug>(dirbuf_up)', { buffer = true })

-- Unmap built-in ZZ keybinding.
vim.keymap.set('n', 'ZZ', '<NOP>', { buffer = true })
