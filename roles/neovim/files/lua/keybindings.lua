--   ██                           ██      ██              ██ ██
--  ░██                          ░██     ░░              ░██░░
--  ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
--  ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
--  ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
--  ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
--  ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
--  ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
--                   ░░░░░                                              ░░░░░

local M = {} -- All functions that need to be exported should go in this table.
local buf_set_keymap = require("util").buf_set_keymap
local set_keymap     = require("util").set_keymap
local which_key      = require('util').which_key()


-- -- Dealing with word wrap:
-- -- If cursor is inside very long line in the file than wraps around
-- -- several rows on the screen, then 'j' key moves you to the next line
-- -- in the file, but not to the next row on the screen under your
-- -- previous position as in other editors.  These bindings fixes this.
-- vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", {noremap = true, expr = true, silent = true})
-- vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", {noremap = true, expr = true, silent = true})

-- Hop (Easymotion) {{{
function M.hop()
   local opts = { noremap=true, silent=false }

   set_keymap('n', ';w', 'Easymotion forward word', "<cmd>HopWordAC<CR>", opts)
   set_keymap('v', ';w', 'easymotion forward word', "<cmd>HopWordAC<CR>", opts)
   set_keymap('n', ';b', 'Easymotion bacward word', "<cmd>HopWordBC<CR>", opts)
   set_keymap('v', ';b', 'Easymotion bacward word', "<cmd>HopWordBC<CR>", opts)

   set_keymap('n', ';j', "Easymotion line up",   "<cmd>HopLineAC<CR>", opts)
   set_keymap('v', ';j', "Easymotion line up",   "<cmd>HopLineAC<CR>", opts)
   set_keymap('n', ';k', "Easymotion line down", "<cmd>HopLineBC<CR>", opts)
   set_keymap('v', ';k', "Easymotion line down", "<cmd>HopLineBC<CR>", opts)

   set_keymap('n', 's', "Easymotion char", "<cmd>HopChar1<CR>", opts)
   set_keymap('v', 's', "Easymotion char", "<cmd>HopChar1<CR>", opts)

end -- }}}

-- Telescope {{{
function M.telescope()
   local opts = { noremap=true, silent=false }

   set_keymap('n', '<leader>ff', 'Telescope: Find files', '<cmd>Telescope find_files<cr>', opts)
   set_keymap('n', '<leader>fg', 'Telescope: Live grep',  '<cmd>Telescope live_grep<cr>',  opts)
   set_keymap('n', '<leader>fb', 'Telescope: Buffers',    '<cmd>Telescope buffers<cr>',    opts)
   set_keymap('n', '<leader>fh', 'Telescope: Help tags',  '<cmd>Telescope help_tags<cr>',  opts)

end -- }}}

-- LSP {{{
function M.lspconfig (bufnr)

   -- Mappings options.
   local opts = { noremap=true, silent=false }

   which_key.register({ ['<leader>l'] = {name = 'LSP'}  }, {mode = 'n'})

   -- Lspconfig bindings {{{

   -- See `:help vim.lsp.*` for documentation on any of the below functions
   buf_set_keymap(bufnr, 'n', '<F2>', 'LSP: go to definition', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
   buf_set_keymap(bufnr, 'n', '<S-F2>', 'LSP: go to declaration', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)

   buf_set_keymap(bufnr, 'n', 'K', 'LSP: Hover doc', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)

   -- buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)

   buf_set_keymap(bufnr, 'n', '<leader>lt', 'Type definition', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)

   -- buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)

   buf_set_keymap(bufnr, 'n', '<leader>le', 'Show errors', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)

   -- buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
   -- buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
   -- buf_set_keymap(bufnr, "n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

   --}}}

   -- Lspsaga {{{

   -- lsp provider to find the cursor word definition and reference
   buf_set_keymap(bufnr, "n", "<leader>le", 'show line diagnostics', "<cmd>Lspsaga lsp_finder<CR>", opts)

   -- wk.register({ ['<leader>c'] = {name = 'LSP: Code action'}  }, {mode = 'n'})
   -- wk.register({ ['<leader>c'] = {name = 'LSP: Code action'}  }, {mode = 'v'})

   buf_set_keymap(bufnr, "n", "<leader>lc", 'Code action', '<cmd>Lspsaga code_action<CR>', opts)
   buf_set_keymap(bufnr, "v", "<leader>lc", 'LSP: Range code action', "<cmd><C-U>Lspsaga range_code_action<CR>", opts)

   -- rename
   buf_set_keymap(bufnr, "n",         "gr", "Rename", "<cmd>Lspsaga rename<CR>", opts)
   buf_set_keymap(bufnr, "n", "<leader>lr", "Rename", "<cmd>Lspsaga rename<CR>", opts)

   -- preview definition
   buf_set_keymap(bufnr, "n", "<leader>ld", "Preview definition", "<cmd>Lspsaga preview_definition<CR>", opts)

   -- -- hover doc
   -- buf_set_keymap(bufnr, "n", "K", "Hover doc", "<cmd>Lspsaga hover_doc<CR>", opts)

   -- show signature help
   buf_set_keymap(bufnr, "n", "<C-k>", "LSP: Show sinature help", "<cmd>Lspsaga signature_help<CR>", opts)

   -- Show Diagnostics
   buf_set_keymap(bufnr, "n", "<leader>le", "Show diagnostic", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
   -- only show diagnostic if cursor is over the area
   -- buf_set_keymap(bufnr, "n", "<leader>le", "Show diagnostic", "<cmd>lua require'lspsaga.diagnostic'.show_cursor_diagnostics()<CR>", opts)

   -- -- jump diagnostic
   -- buf_set_keymap(bufnr, "n", "]e", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
   -- buf_set_keymap(bufnr, "n", "[e", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)


   -- -- scroll down / up inside different preview windows
   -- buf_set_keymap(bufnr, "n", "<C-f>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>", opts)
   -- buf_set_keymap(bufnr, "n", "<C-b>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>", opts)

   --}}}

end --}}}

-- Treesitter {{{
function M.treesitter_textobjects()
   return {
      select = { --{{{
         enable = true,
         keymaps = {
            -- You can use the capture groups defined in textobjects.scm
            ['af'] = '@function.outer',
            ['if'] = '@function.inner',
            -- ['aC'] = '@class.outer',
            -- ['iC'] = '@class.inner',
            ['aC'] = '@conditional.outer',
            ['iC'] = '@conditional.inner',
            ['ic'] = '@comment',
            -- ['ic'] = '@comment.inner',
            -- ['ac'] = '@comment.outer',
            ['ae'] = '@block.outer',
            ['ie'] = '@block.inner',
            ['al'] = '@loop.outer',
            ['il'] = '@loop.inner',
            ['is'] = '@statement.inner',
            ['as'] = '@statement.outer',
            ['am'] = '@call.outer',
            ['im'] = '@call.inner',

            -- -- or you use the queries from supported languages with textobjects.scm
            -- ['iF'] = {
            --   python = '(function_definition) @function',
            --   cpp = '(function_definition) @function',
            --   c = '(function_definition) @function',
            --   java = '(method_declaration) @function'
            -- }
         }
      }, --}}}
      move = { --{{{
         enable = true,
         set_jumps = true, -- whether to set jumps in the jumplist
         goto_next_start = {
            ["]m"] = "@function.outer",
            ["]]"] = "@class.outer",
         },
         goto_next_end = {
            ["]M"] = "@function.outer",
            ["]["] = "@class.outer",
         },
         goto_previous_start = {
            ["[m"] = "@function.outer",
            ["[["] = "@class.outer",
         },
         goto_previous_end = {
            ["[M"] = "@function.outer",
            ["[]"] = "@class.outer",
         },
      }, --}}}
   }
end --}}}

-- nvim-tree {{{
function M.nvim_tree()
   local opts = { noremap=true, silent=false }

   set_keymap('n', '<F3>', 'Open lile-explorer', "<cmd>NvimTreeToggle<CR>", opts)

   local tree_cb = require'nvim-tree.config'.nvim_tree_callback
   vim.g.nvim_tree_bindings = {
      { key = '?', cb = tree_cb("toggle_help") }  -- help UI
   }

end --}}}

-- Barbar (tabline) {{{
function M.barbar()

   local set_keymap = vim.api.nvim_set_keymap
   local opts = { noremap=true, silent=true }

   -- Move to previous/next
   set_keymap('n', '<A-,>', '<cmd>BufferPrevious<CR>', opts)
   set_keymap('n', '<A-.>', '<cmd>BufferNext<CR>', opts)
   -- Re-order to previous/next
   set_keymap('n', '<A-<>', '<cmd>BufferMovePrevious<CR>', opts)
   set_keymap('n', '<A->>', '<cmd>BufferMoveNext<CR>', opts)
   -- Goto buffer in position...
   set_keymap('n', '<A-1>', '<cmd>BufferGoto 1<CR>', opts)
   set_keymap('n', '<A-2>', '<cmd>BufferGoto 2<CR>', opts)
   set_keymap('n', '<A-3>', '<cmd>BufferGoto 3<CR>', opts)
   set_keymap('n', '<A-4>', '<cmd>BufferGoto 4<CR>', opts)
   set_keymap('n', '<A-5>', '<cmd>BufferGoto 5<CR>', opts)
   set_keymap('n', '<A-6>', '<cmd>BufferGoto 6<CR>', opts)
   set_keymap('n', '<A-7>', '<cmd>BufferGoto 7<CR>', opts)
   set_keymap('n', '<A-8>', '<cmd>BufferGoto 8<CR>', opts)
   set_keymap('n', '<A-9>', '<cmd>BufferLast<CR>', opts)
   -- Close buffer
   set_keymap('n', '<A-c>', '<cmd>BufferClose<CR>', opts)
   -- Wipeout buffer
   --                          <cmd>BufferWipeout<CR>
   -- Close commands
   --                          <cmd>BufferCloseAllButCurrent<CR>
   --                          <cmd>BufferCloseBuffersLeft<CR>
   --                          <cmd>BufferCloseBuffersRight<CR>
   -- Magic buffer-picking mode
   set_keymap('n', '<C-s>',    '<cmd>BufferPick<CR>', opts)
   -- Sort automatically by...
   set_keymap('n', '<Space>bd', '<cmd>BufferOrderByDirectory<CR>', opts)
   set_keymap('n', '<Space>bl', '<cmd>BufferOrderByLanguage<CR>', opts)

   -- Other:
   -- :BarbarEnable - enables barbar (enabled by default)
   -- :BarbarDisable - very bad command, should never be used

end -- }}}

return M

-- vim: fdm=marker
