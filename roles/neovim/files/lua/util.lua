local which_key_available, which_key = pcall(require, "which-key")
local M = {}

--- Check if file exists
function M.is_file_exists(path)
   local f = io.open(path, "r")
   if f ~= nil then
      io.close(f)
      return true
   else
      return false
   end
end


--------------------- Keybindings ---------------------

-- The wrapper function around the 'which-key.register()' function.
-- Doesn't throw an error if 'which-key' plugin doesn't available.
function _G.which_key(opt)
   if not which_key_available then return end
   which_key.register(opt)
end

--- Set global keymap and register its description in 'which-key' plugin.
---@param mode string
---@param lhs string left hand side
---@param description string
---@param rhs string right hand side
---@param opts? table Default values are:
--```lua
--{ noremap = true, silent = true }
--```
function M.map(mode, lhs, description, rhs, opts)
   local options = { noremap = true, silent = true }
   options = vim.tbl_deep_extend("force", options, opts or {})

   vim.api.nvim_set_keymap(mode, lhs, rhs, options)

   if which_key_available then
      which_key.register{
         [lhs] = { description, mode = mode }
      }
   end
end


-- Set buffer keymap and register its description in 'which-key' plugin.
---@param bufnr number
---@param mode string
---@param lhs string
---@param description string
---@param rhs string
---@param opts? table Default values are:
--```lua
--{ noremap = true, silent = true }
--```
function M.buf_map(bufnr, mode, lhs, description, rhs, opts)
   local options = { noremap = true, silent = true }
   options = vim.tbl_deep_extend("force", options, opts or {})

   vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, options)

   if which_key_available then
      which_key.register{
         [lhs] = { description, mode = mode, buffer = bufnr }
      }
   end
end


return M
