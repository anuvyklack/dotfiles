local util = {}

function util.warn(msg)
   vim.schedule(function()
      vim.notify_once(msg, vim.log.levels.WARN)
   end)
end

---@class util.Keymap
---@field cmd function
---@field set function
util.keymap = {} ---@type util.Keymap

---Wrapper around vim.keymap.set() function. It accepts in {opts} parameter
---table next additional keys:
--- - **requires**: (string) Package that should be available to set this keymap.
--- - **ft_ignore**: (string | string[]) Filetype or list of filetypes for which
---   this keymap won't be set.
util.keymap.set = function (...)
   local decision = true -- The decision to set keymap or not.
   local opts = select(-1, ...)
   if type(opts) == 'table' then
      if opts.requires then
         decision, _ = pcall(require, opts.requires)
         opts.requires = nil
      end
      if opts.ft_ignore then
         if type(opts.ft_ignore) == 'string' then
            opts.ft_ignore = { opts.ft_ignore }
         end
         decision = not vim.tbl_contains(opts.ft_ignore, vim.bo.filetype)
         opts.ft_ignore = nil
      end
   end
   if decision then vim.keymap.set(...) end
end

local which_key = prequire('which-key')

---The wrapper around the 'which-key.register()' function.
---Doesn't throw an error if 'which-key' plugin doesn't available.
util.which_key = setmetatable({}, {
   __call = function (_, ...)
      which_key.register(...)
   end
})

---Setup WhichKey group name
---@param lhs string
---@param name string
util.which_key.name = function(mode, lhs, name)
   util.which_key({
      [lhs] = { name = name }
   }, { mode = mode })
end

return util
