local builtin = prequire 'telescope.builtin'
local custom_actions = require 'anuvyklack.telescope.custom_actions'

local M = {}

M.grep = function(opts)
   opts = opts or {}
   local search = vim.fn.input 'Grep >'
   local cwd = opts.use_buffer_cwd and vim.fn.expand '%:p:h' or nil
   if cwd == '' then
      -- we expanded to nothing - default to cwd
      cwd = vim.loop.cwd()
   end
   if search ~= '' then
      builtin.grep_string {
         cwd = cwd,
         only_sort_text = true,
         search = search,
         use_regex = true,
         disable_coordinates = true,
         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               mirror = true,
               preview_height = 0.5,
            },
         },
         mappings = {
            i = { ['<CR>'] = custom_actions.auto_multi_selection_open_qflist },
            n = { ['<CR>'] = custom_actions.auto_multi_selection_open_qflist }
         }
      }
   else
      builtin.live_grep {
         cwd = cwd,
         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               mirror = true,
               preview_height = 0.5,
            },
         },
         mappings = {
            i = { ['<CR>'] = custom_actions.auto_multi_selection_open_qflist },
            n = { ['<CR>'] = custom_actions.auto_multi_selection_open_qflist }
         }
      }
   end
end

-- Git -------------------------------------------------------------------------

M.git_status = builtin.git_status

M.git_stash = function()
   builtin.git_stash {
   --    layout_strategy = 'flex',
   --    layout_config = {
   --       flex = {
   --          flip_columns = 161, -- half 27" monitor, scientifically calculated
   --       },
   --       horizontal = {
   --          preview_cutoff = 0,
   --          preview_width = { padding = 17 },
   --       },
   --       vertical = {
   --          preview_cutoff = 0,
   --          preview_height = { padding = 5 },
   --       },
   --    },
   }
end

-- LSP -------------------------------------------------------------------------

M.definitions = builtin.lsp_definitions

M.references = builtin.lsp_references

M.implementations = builtin.lsp_implementations

M.workspace_symbols = function()
   local query = vim.fn.input 'Query >'
   if query ~= '' then
      vim.cmd('Telescope lsp_workspace_symbols query=' .. query)
   else
      builtin.lsp_workspace_symbols()
   end
end

M.document_symbols = function()
   local symbols = {
      'All',
      'Variable',
      'Function',
      'Constant',
      'Class',
      'Property',
      'Method',
      'Enum',
      'Interface',
      'Boolean',
      'Number',
      'String',
      'Array',
      'Constructor',
   }

   vim.ui.select(symbols, { prompt = 'Select which symbol' }, function(item)
      if item == 'All' then
         builtin.lsp_document_symbols()
      else
         builtin.lsp_document_symbols { symbols = item }
      end
   end)
end

--------------------------------------------------------------------------------

return M
