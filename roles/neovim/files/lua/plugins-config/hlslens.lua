local set_keymap = vim.api.nvim_set_keymap
local wk = require("which-key")

local opts = { noremap=true, silent=true }

set_keymap('n', 'n', "<Cmd>execute('normal! '.v:count1.'n')<CR><Cmd>lua require('hlslens').start()<CR>", opts)
set_keymap('n', 'N', "<Cmd>execute('normal! '.v:count1.'N')<CR><Cmd>lua require('hlslens').start()<CR>", opts)

set_keymap('n', '*', "*<Cmd>lua require('hlslens').start()<CR>", opts)
set_keymap('n', '#', "#<Cmd>lua require('hlslens').start()<CR>", opts)

set_keymap('n', 'g*', "g*<Cmd>lua require('hlslens').start()<CR>", opts)
set_keymap('n', 'g#', "g#<Cmd>lua require('hlslens').start()<CR>", opts)

wk.register({
   ["g*"] = "which_key_ignore",
   ["g#"] = "which_key_ignore",
})
