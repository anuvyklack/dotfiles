local util = {}

util.keymap = {}

---Wrapper around vim.keymap.set() function. It accepts in {opts} parameter
---table next additional keys:
--- - **requires** (string) - package that should be available to set this keymap
--- - **ft_ignore** (string | list) - filetype or list of filetypes for which this keymap won't be set
util.keymap.set = function (...)
   local decision = true -- To set keymap or not.
   local opts = select(-1, ...)
   if type(opts) == 'table' then
      if opts.requires then
         decision, _ = pcall(require, opts.requires)
         opts.requires = nil
      end
      if opts.ft_ignore then
         if type(opts.ft_ignore) == "string" then
            opts.ft_ignore = { opts.ft_ignore }
         end
         decision = not vim.tbl_contains(opts.ft_ignore, vim.bo.filetype)
         opts.ft_ignore = nil
      end
   end
   if decision then vim.keymap.set(...) end
end

---The wrapper around the 'which-key.register()' function.
---Doesn't throw an error if 'which-key' plugin doesn't available.
util.which_key = setmetatable({}, {
   __call = function (_, ...) -- the first argument is self
      local available, which_key = pcall(require, "which-key")
      if available then which_key.register(...) end
   end
})

---Setup WhichKey group name.
---@param lhs string
---@param name string
util.which_key.name = function(mode, lhs, name)
   util.which_key({
      [lhs] = { name = name }
   }, { mode = mode })
end

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

return util
