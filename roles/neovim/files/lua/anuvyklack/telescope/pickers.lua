local builtin = prequire 'telescope/builtin'
local custom_actions = require 'anuvyklack/telescope/custom_actions'

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

M.buffer_diagnostics = function() builtin.diagnostics { bufnr = 0 } end

M.workspace_diagnostics = builtin.diagnostics

M.definitions = builtin.lsp_definitions

M.references = builtin.lsp_references

M.buffer_references = function(opts)
   opts = opts or {}
   local params = vim.lsp.util.make_position_params(opts.winnr)
   params.context = { includeDeclaration = true }

   vim.lsp.buf_request(opts.bufnr, "textDocument/references", params, function(err, result, ctx, _config)
      if err then
         vim.api.nvim_err_writeln("Error when finding references: " .. err.message)
         return
      end

      local locations = {}
      if result then
         local filtered_result = result
         local buf_uri = vim.uri_from_bufnr(0)
         filtered_result = vim.tbl_filter(function(location)
            return (location.uri or location.targetUri) == buf_uri
         end, result)

         locations = vim.lsp.util.locations_to_items(
            filtered_result,
            vim.lsp.get_client_by_id(ctx.client_id).offset_encoding
         ) or {}
      end

      if vim.tbl_isempty(locations) then
         return
      end

      pickers.new(opts, {
         prompt_title = "LSP References (filtered)",
         finder = finders.new_table {
            results = locations,
            entry_maker = opts.entry_maker or make_entry.gen_from_quickfix(opts),
         },
         previewer = conf.qflist_previewer(opts),
         sorter = conf.generic_sorter(opts),
      }):find()
   end)
end

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
