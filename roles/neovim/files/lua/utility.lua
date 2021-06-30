
local M = {}

-- Check if lua module is available.
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


-- Check if file exists.
function M.is_file_exists(path)
  local f=io.open(path,"r")
  if f~=nil then
    io.close(f)
    return true
  else
    return false
  end
end


return M
