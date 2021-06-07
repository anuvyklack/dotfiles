-- WARNING This file must be loaded before nvim-treesitter plugin.

-- Function that checks if file exists.
local is_file_exists = require("utility").is_file_exists


-- We manually load treesitter queries, that overloads the default ones.
-- Taken from:
-- https://github.com/nvim-treesitter/nvim-treesitter/issues/839#issuecomment-850027287
local function set_ft_query(ft, type)
  local path = (vim.fn.stdpath("config") .. ("/queries/" .. ft .. "/" .. type .. ".scm"))

  if is_file_exists(path) then
    local query = vim.fn.join(vim.fn.readfile(path), "\n")
    require("vim.treesitter.query").set_query(ft, type, query)
  else
    print("File " .. path .. " doesn't exists!")
  end
end

set_ft_query("cpp", "folds")

-- local vim_ts_queries = require("vim.treesitter.query")
-- vim_ts_queries.set_query("c", "folds", get_ft_query("c", "folds"))
-- vim_ts_queries.set_query("cpp", "folds", get_ft_query("cpp", "folds"))
