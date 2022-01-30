local which_key_available, which_key = pcall(require, "which-key")
local util = {}

---Check if file exists.
---@param path string
function util.file_exists(path)
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

---Set keymap and register its description in 'which-key' plugin.  If `opts`
---table contains `desc` key, its value will be registred in 'which-key' plugin.
---Also see: :help vim.keymap.set()
---@param mode string|string[]
---@param lhs string left hand side
---@param rhs string|function right hand side
---@param opts? table
function util.keymap.set(mode, lhs, rhs, opts)
   if vim.keymap.set then
      vim.keymap.set(mode, lhs, rhs, opts)
   else
      if type(rhs) == "function" then
         _G.anuvyklack = _G.anuvyklack or {}
         _G.anuvyklack.functions = _G.anuvyklack.functions or { next_index = 1 }
         local next_index = _G.anuvyklack.functions.next_index
         local fun = 'f' .. next_index
         _G.anuvyklack.functions[fun] = rhs
         rhs = string.format('lua _G.anuvyklack.functions[%s]', fun)
         _G.anuvyklack.functions.next_index = next_index + 1
      end
      for _, m in ipairs(mode) do
         if opts.buffer then
            vim.api.nvim_buf_set_keymap(opts.buffer, m, lhs, rhs, opts)
         else
            vim.api.nvim_set_keymap(m, lhs, rhs, opts)
         end
      end
   end

   if opts and opts.desc then
      _G.which_key({
         [lhs] = { opts.desc, mode = mode }
      })
   end
end

--------------------------------------------------------------------------------

return util
