local which_key_available, which_key = pcall(require, "which-key")
local util = {}

---Check if file exists
function util.is_file_exists(path)
   local f = io.open(path, "r")
   if f ~= nil then
      io.close(f)
      return true
   else
      return false
   end
end


-- Keybindings -----------------------------------------------------------------

util.keymap = {}

---The wrapper around the 'which-key.register()' function.
---Doesn't throw an error if 'which-key' plugin doesn't available.
_G.which_key = setmetatable({}, {
   __call = function (_, ...) -- the first argument is self
      if which_key_available then which_key.register(...) end
   end
})

---Setup group name
---@param lhs string
---@param name string
_G.which_key.name = function(mode, lhs, name)
   _G.which_key { [lhs] = { name = name, mode = mode } }
end

---Set keymap and register its description in 'which-key' plugin.  Any of
---`description` or `opts` arguments may be ommited. So it is possible to pass
---`opts` table without `description` string.  See: :help vim.keymap.set()
---@param mode string|string[]
---@param lhs string left hand side
---@param rhs string|function right hand side
---@param description? string
---@param opts? table
function util.keymap.set(mode, lhs, rhs, description, opts)
   -- local options = { noremap = true, silent = true }
   -- opts = vim.tbl_deep_extend("force", options, opts or {})

   if type(description) == 'table' then
      opts = description
      description = nil
   end

   vim.keymap.set(mode, lhs, rhs, opts)

   if description then
      _G.which_key {
         [lhs] = { description, mode = mode }
      }
   end
end

--------------------------------------------------------------------------------

return util
