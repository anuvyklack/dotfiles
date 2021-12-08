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

local mapx_available, mapx = pcall(require, 'mapx')

if mapx_available then
   mapx.setup({ global = true, whichkey = true })

   -- local mapx = require'mapx'.setup({ global = true, whichkey = true })

   -- Dealing with word wrap:
   -- If cursor is inside very long line in the file than wraps around
   -- several rows on the screen, then 'j' key moves you to the next line
   -- in the file, but not to the next row on the screen under your
   -- previous position as in other editors.  These bindings fixes this.
   nnoremap('k', "v:count ? 'k' : 'gk'", {silent = true, expr = true})
   nnoremap('j', "v:count ? 'j' : 'gj'", {silent = true, expr = true})

end


-- Hop (Easymotion) {{{
function M.hop()
   if not mapx_available then return end

   nnoremap(';w', '<cmd>HopWordAC<CR>', 'Easymotion forward word')
   vnoremap(';w', '<cmd>HopWordAC<CR>', 'Easymotion forward word')

   nnoremap(';b', '<cmd>HopWordBC<CR>', 'Easymotion bacward word')
   vnoremap(';b', '<cmd>HopWordBC<CR>', 'Easymotion bacward word')

   nnoremap(';j', '<cmd>HopLineAC<CR>', 'Easymotion line up')
   vnoremap(';j', '<cmd>HopLineAC<CR>', 'Easymotion line up')
   nnoremap(';k', '<cmd>HopLineBC<CR>', 'Easymotion line down')
   vnoremap(';k', '<cmd>HopLineBC<CR>', 'Easymotion line down')

   nnoremap('s', '<cmd>HopChar1<CR>', 'Easymotion char')
   vnoremap('s', '<cmd>HopChar1<CR>', 'Easymotion char')

end -- }}}

-- Telescope {{{
function M.telescope()
   if not mapx_available then return end

   nnoremap('<C-;>', '<cmd>Telescope commands<CR>',        'Execute command')
   nnoremap('q:',    '<cmd>Telescope command_history<CR>', 'Command-line history')
   nnoremap('q/',    '<cmd>Telescope search_history<CR>',  'Search history')
   nnoremap('q?',    '<cmd>Telescope search_history<CR>',  'Search history')

   nnoremap('z=',    '<cmd>Telescope spell_suggest<CR>',   'Spell Suggest')

   mapx.nname('<leader>f', 'Telescope')

   nnoremap('<leader>ff', '<cmd>Telescope find_files<CR>', 'Find files')
   nnoremap('<leader>fg', '<cmd>Telescope live_grep<CR>',  'Live grep')
   nnoremap('<leader>fb', '<cmd>Telescope buffers<CR>',    'Buffers')
   nnoremap('<leader>fh', '<cmd>Telescope help_tags<CR>',  'Help tags')
   nnoremap('<leader>fp', '<cmd>Telescope projects<CR>',   'Projects')
   nnoremap('<leader>fo', '<cmd>Telescope vim_options<CR>','Vim Options')

   nnoremap('<leader>fm', '<cmd>MarksListBuf<CR>',         'Marks')

   nnoremap('<C-/>', '<cmd>Telescope current_buffer_fuzzy_find<CR>', 'Search in buffer')

end -- }}}

-- LSP {{{
function M.lspconfig(bufnr)
   if not mapx_available then return end

   mapx.nname('<leader>l', 'LSP')

   -- Lspconfig bindings {{{

   -- See `:help vim.lsp.*` for documentation on any of the below functions
   nnoremap('<F2>',   '<Cmd>lua vim.lsp.buf.definition()<CR>',  {buffer = bufnr}, 'LSP: go to definition')
   nnoremap('<S-F2>', '<Cmd>lua vim.lsp.buf.declaration()<CR>', {buffer = bufnr}, 'LSP: go to declaration')

   nnoremap('K', '<Cmd>lua vim.lsp.buf.hover()<CR>', {buffer = bufnr}, 'LSP: Hover doc')

   -- buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>')
   -- buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
   -- buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>')
   -- buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>')
   -- buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>')

   nnoremap('<leader>lt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', {buffer = bufnr}, 'LSP: Type definition')

   -- buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>')
   -- buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>')
   -- buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>')

   nnoremap('<leader>le', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', {buffer = bufnr}, 'Show errors')

   -- buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')
   -- buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
   -- buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>')
   -- buf_set_keymap(bufnr, "n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>")

   --}}}

   -- Lspsaga {{{

   -- lsp provider to find the cursor word definition and reference
   nnoremap("<leader>le", "<cmd>Lspsaga lsp_finder<CR>", {buffer = bufnr}, 'Show line diagnostics')

   -- wk.register({ ['<leader>c'] = {name = 'LSP: Code action'}  }, {mode = 'n'})
   -- wk.register({ ['<leader>c'] = {name = 'LSP: Code action'}  }, {mode = 'v'})

   nnoremap('<leader>la', '<cmd>Lspsaga code_action<CR>',            {buffer = bufnr}, 'Code action')
   vnoremap('<leader>la', '<cmd><C-U>Lspsaga range_code_action<CR>', {buffer = bufnr}, 'LSP: Range code action')

   -- rename
   nnoremap(        'gr', '<cmd>Lspsaga rename<CR>', {buffer = bufnr}, 'Rename')
   nnoremap('<leader>lr', '<cmd>Lspsaga rename<CR>', {buffer = bufnr}, 'Rename')

   -- preview definition
   nnoremap('<leader>ld', '<cmd>Lspsaga preview_definition<CR>', {buffer = bufnr}, 'Preview definition')

   -- -- hover doc
   -- buf_set_keymap(bufnr, 'n', 'K', 'Hover doc', '<cmd>Lspsaga hover_doc<CR>')

   -- show signature help
   nnoremap('<C-k>', '<cmd>Lspsaga signature_help<CR>', {buffer = bufnr}, 'LSP: Show sinature help')

   -- Show Diagnostics
   nnoremap('<leader>le', '<cmd>Lspsaga show_line_diagnostics<CR>', {buffer = bufnr}, 'Show diagnostic')
   -- only show diagnostic if cursor is over the area
   -- buf_set_keymap(bufnr, 'n', '<leader>le', 'Show diagnostic', "<cmd>lua require'lspsaga.diagnostic'.show_cursor_diagnostics()<CR>")

   -- -- jump diagnostic
   -- buf_set_keymap(bufnr, 'n', ']e', '<cmd>Lspsaga diagnostic_jump_next<CR>')
   -- buf_set_keymap(bufnr, 'n', '[e', '<cmd>Lspsaga diagnostic_jump_prev<CR>')


   -- -- scroll down / up inside different preview windows
   -- buf_set_keymap(bufnr, 'n', '<C-f>', "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>")
   -- buf_set_keymap(bufnr, 'n', '<C-b>', "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>")

   --}}}

end --}}}

-- nvim-compe {{{
function M.nvim_compe()
   if not mapx_available then return end

   local opts = {noremap = true, silent = true, expr = true}

   vim.api.nvim_set_keymap('i', '<C-Space>', "compe#complete()",              opts)
   -- vim.api.nvim_set_keymap('i', '<CR>',      "compe#confirm('<CR>')",         opts)
   vim.api.nvim_set_keymap('i', '<C-e>',     "compe#close('<C-e>')",          opts)
   vim.api.nvim_set_keymap('i', '<C-f>',     "compe#scroll({ 'delta': +4 })", opts)
   vim.api.nvim_set_keymap('i', '<C-d>',     "compe#scroll({ 'delta': -4 })", opts)

end -- }}}

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
   if not mapx_available then return end

   nnoremap('<F3>', '<cmd>NvimTreeToggle<CR>', 'Open file-explorer')

   -- local tree_cb = require'nvim-tree.config'.nvim_tree_callback
   -- vim.g.nvim_tree_bindings = {
   --    { key = '?', cb = tree_cb("toggle_help") }  -- help UI
   -- }

end --}}}

-- nnn file manager {{{
function M.nnn()
   if not mapx_available then return end

   nnoremap('<F4>', '<cmd>NnnPicker<CR>', 'Open file-explorer')
   -- nnoremap('<F3>', '<cmd>NnnExplorer<CR>', 'Open file-explorer')

end --}}}

-- Barbar (tabline) {{{
function M.barbar()
   if not mapx_available then return end

   local opts = { silent = true }

   -- Move to previous/next
   nnoremap('<A-,>', '<cmd>BufferPrevious<CR>', opts)
   nnoremap('<A-.>', '<cmd>BufferNext<CR>', opts)
   -- Re-order to previous/next
   nnoremap('<A-<>', '<cmd>BufferMovePrevious<CR>', opts)
   nnoremap('<A->>', '<cmd>BufferMoveNext<CR>', opts)
   -- Goto buffer in position...
   nnoremap('<A-1>', '<cmd>BufferGoto 1<CR>', opts)
   nnoremap('<A-2>', '<cmd>BufferGoto 2<CR>', opts)
   nnoremap('<A-3>', '<cmd>BufferGoto 3<CR>', opts)
   nnoremap('<A-4>', '<cmd>BufferGoto 4<CR>', opts)
   nnoremap('<A-5>', '<cmd>BufferGoto 5<CR>', opts)
   nnoremap('<A-6>', '<cmd>BufferGoto 6<CR>', opts)
   nnoremap('<A-7>', '<cmd>BufferGoto 7<CR>', opts)
   nnoremap('<A-8>', '<cmd>BufferGoto 8<CR>', opts)
   nnoremap('<A-9>', '<cmd>BufferLast<CR>', opts)
   -- Close buffer
   nnoremap('<A-c>', '<cmd>BufferClose<CR>', opts)
   -- Wipeout buffer
   --                          <cmd>BufferWipeout<CR>
   -- Close commands
   --                          <cmd>BufferCloseAllButCurrent<CR>
   --                          <cmd>BufferCloseBuffersLeft<CR>
   --                          <cmd>BufferCloseBuffersRight<CR>
   -- Magic buffer-picking mode
   nnoremap('<C-s>',    '<cmd>BufferPick<CR>', opts)
   -- Sort automatically by...
   nnoremap('<Space>bd', '<cmd>BufferOrderByDirectory<CR>', opts)
   nnoremap('<Space>bl', '<cmd>BufferOrderByLanguage<CR>', opts)

   -- Other:
   -- :BarbarEnable - enables barbar (enabled by default)
   -- :BarbarDisable - very bad command, should never be used

end -- }}}

function M.asterisks()
   if not mapx_available then return end
   map('*',  '<Plug>(asterisk-z*)')
   map('#',  '<Plug>(asterisk-z#)')
   map('g*', '<Plug>(asterisk-gz*)', ':help gstar')
   map('g#', '<Plug>(asterisk-gz#)', ':help g#')
end

return M

-- vim: fdm=marker
