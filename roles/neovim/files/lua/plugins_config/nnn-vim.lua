local available, nnn = pcall(require, 'nnn')
if not available then return end


-- local function copy_to_clipboard(lines)
--    local joined_lines = table.concat(lines, "\n")
--    vim.fn.setreg("+", joined_lines)
-- end

nnn.setup({
   command = "nnn -o -C",
   layout = {
      window = {
         width = 0.6,
         height = 0.7,
         highlight = 'Comment'
      }
   },
   replace_netrw = 1,
   -- set_default_mappings = 0,
   -- action = {
   --    ["<c-t>"] = "tab split",
   --    ["<c-s>"] = "split",
   --    ["<c-v>"] = "vsplit",
   --    ["<c-o>"] = copy_to_clipboard,
   -- },
})

require('keybindings').nnn()
