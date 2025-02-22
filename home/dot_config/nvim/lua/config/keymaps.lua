-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua

local keymap = vim.keymap

keymap.set({'n','x'}, '<BS>', ':')

keymap.set({ "n", "x", "o" }, "gh", "^", { desc = "beginning of line" })
keymap.set({ "n", "x", "o" }, "gl", "$", { desc = "end of line" })

keymap.set('n', 'zr', 'zR')
keymap.set('n', 'zm', 'zM')

keymap.set("n", "gq", "gw")
