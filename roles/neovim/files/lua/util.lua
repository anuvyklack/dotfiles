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

local which_key_available, which_key = pcall(require, "which-key")

---The wrapper around the 'which-key.register()' function.
---Doesn't throw an error if 'which-key' plugin doesn't available.
util.which_key = setmetatable({}, {
   __call = function (_, ...) -- the first argument is self
      if which_key_available then which_key.register(...) end
   end
})

---Setup group name
---@param lhs string
---@param name string
util.which_key.name = function(mode, lhs, name)
   util.which_key({
      [lhs] = { name = name, mode = mode }
   })
end

return util
