local available, nnn = pcall(require, 'nnn')
if not available then return end

vim.env.NNN_COLORS='3214'
vim.env.NNN_PLUG = 'z:autojump'

-- local function copy_to_clipboard(lines)
--    local joined_lines = table.concat(lines, "\n")
--    vim.fn.setreg("+", joined_lines)
-- end

nnn.setup({
   replace_netrw = 0,
   -- command = "nnn -oCG",
   command = "nnn -oC", -- -G - show git status
   session = 'local', -- Use n³ sessions to remember the location when it is reopened.
   -- explorer_layout = '25%',
   layout = {
      window = {
         width = 0.6,
         height = 0.7,
         highlight = 'Comment'
      }
   },
   set_default_mappings = 0,
   action = {
      ["<C-t>"] = "tab split",
      ["<C-s>"] = "split",
      ["<C-v>"] = "vsplit",
      -- ["<C-o>"] = copy_to_clipboard,
   },
})

-- WARNING
-- Some keybindings are in the ~/.config/nvim/after/ftplugin/nnn.vim
require('keybindings').nnn()
