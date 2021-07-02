-- WARNING This file must be loaded before nvim-treesitter plugin.

-- Function that checks if file exists.
local is_file_exists = require("utility").is_file_exists


-- Manually load treesitter queries to overload the default ones.
-- https://github.com/nvim-treesitter/nvim-treesitter/issues/839#issuecomment-850027287
local function set_ft_query(ft, type)
  local path = (vim.fn.stdpath("config") .. ("/queries/" .. ft .. "/" .. type .. ".scm"))

  -- Exit if there is no such file.
  if not is_file_exists(path) then
    return
  end

  local list_parsers = require("vim.treesitter.health").list_parsers

  for _, parser in pairs(list_parsers()) do
    if string.find(parser, ft) then
      local query = vim.fn.join(vim.fn.readfile(path), "\n")
      require("vim.treesitter.query").set_query(ft, type, query)
    end
  end

end

set_ft_query("cpp", "folds")
