-- Manually load treesitter queries to overload the default ones.
-- https://github.com/nvim-treesitter/nvim-treesitter/issues/839#issuecomment-850027287

if not pcall(require, 'plenary') then return end

local set_query = require("vim.treesitter.query").set_query
local Path = require('plenary.path')
local scan_dir = require('plenary.scandir').scan_dir

local parsers = {}
for _, parser in ipairs(require("vim.treesitter.health").list_parsers()) do
   parsers[vim.fn.fnamemodify(parser, ':t:r')] = true
end

local nvim_config = Path.new(vim.fn.stdpath('config'))
local queries_path = nvim_config / 'queries'
local filetypes = scan_dir(tostring(queries_path), { depth = 1, only_dirs = true, })

for _, ft in ipairs(filetypes) do
   local filetype = vim.fn.fnamemodify(ft, ':t')
   if parsers[filetype] then
      for _, query in ipairs(scan_dir(ft)) do
         local type = vim.fn.fnamemodify(query, ':t:r')
         query = Path.new(query)
         query = table.concat(query:readlines(), '\n')
         set_query(filetype, type, query)
      end
   end
end
