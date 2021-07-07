
local M = {}

-- Check if lua module is available
-- Using as:
-- ```lua
-- if is_module_available("menu") then
--   require("menu")
-- end
-- ```
---@param name string
function M.is_module_available(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end


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

-- Set global keymap and register its description in 'which-key' plugin.
---@param mode string
---@param lhs string
---@param description string
---@param rhs string
---@param opts table
function M.set_keymap(mode, lhs, description, rhs, opts)
  vim.api.nvim_set_keymap(mode, lhs, rhs, opts)

  if M.is_module_available("which-key") then
    require("which-key").register{
      [lhs] = { description, mode = mode}
    }
  end
end


-- Set buffer keymap and register its description in 'which-key' plugin.
---@param bufnr number
---@param mode string
---@param lhs string
---@param description string
---@param rhs string
---@param opts table
function M.buf_set_keymap(bufnr, mode, lhs, description, rhs, opts)
  vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)

  if M.is_module_available("which-key") then
    require("which-key").register{
      [lhs] = { description, mode = mode, buffer = bufnr }
    }
  end
end


-- Returns `require("which-key")` object if available, else return empty table.
---@return 'require("which-key") or {}'
function M.which_key()
  if M.is_module_available("which-key") then
    return require("which-key")
  else
    -- TODO Add metatable which will catch the call of unexisting item.
    return {}
  end
end


return M
