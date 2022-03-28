--  ██                           ██      ██              ██ ██
-- ░██                          ░██     ░░              ░██░░
-- ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
-- ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
-- ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
-- ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
-- ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
-- ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
--                  ░░░░░                                              ░░░░░

local M = {}
local keymap = vim.keymap
local which_key = require('util').which_key

local n, v = 'n', 'v'
local function cmd(command) return table.concat({ '<cmd>', command, '<CR>' }) end

-- Dealing with word wrap:
-- If cursor is inside very long line in the file than wraps around several rows
-- on the screen, then 'j' key moves you to the next line in the file, but not
-- to the next row on the screen under your previous position as in other
-- editors. These bindings fixes this.
keymap.set(n, 'k', function() return vim.v.count > 0 and 'k' or 'gk' end, { silent = true, expr = true })
keymap.set(n, 'j', function() return vim.v.count > 0 and 'j' or 'gj' end, { silent = true, expr = true })

-- Hop (Easymotion) {{{
function M.hop()
   keymap.set({n,v}, ';w', function() vim.cmd('HopWordAC') end, { desc = 'Easymotion forward word' })
   keymap.set({n,v}, ';b', function() vim.cmd('HopWordBC') end, { desc = 'Easymotion bacward word' })

   keymap.set({n,v}, ';j', cmd 'HopLineAC', { desc = 'Easymotion line up' })
   keymap.set({n,v}, ';k', cmd 'HopLineBC', { desc = 'Easymotion line down' })

   keymap.set({n,v}, 's', cmd 'HopChar1', { desc = 'Easymotion char' })

   keymap.set(n, 't', cmd 'HopChar2', { desc = 'Easymotion 2 chars' })
end -- }}}

-- Telescope {{{
function M.telescope()
   keymap.set(n, '<C-;>', cmd 'Telescope commands',        { desc = 'Execute command' })
   keymap.set(n, 'q:',    cmd 'Telescope command_history', { desc = 'Command-line history' })
   keymap.set(n, 'q/',    cmd 'Telescope search_history',  { desc = 'Search history' })
   keymap.set(n, 'q?',    cmd 'Telescope search_history',  { desc = 'Search history' })

   keymap.set(n, 'z=',    cmd 'Telescope spell_suggest',   { desc = 'Spell Suggest' })

   which_key.name(n, '<leader>f', 'Telescope')

   keymap.set(n, '<leader>fa', cmd 'Telescope builtin',    { desc = 'List all pickers' })
   keymap.set(n, '<leader>ff', cmd 'Telescope find_files', { desc = 'Find files' })
   keymap.set(n, '<leader>fg', cmd 'Telescope live_grep',  { desc = 'Live grep' })
   -- keymap.set(n, '<leader>fb', cmd 'Telescope buffers',    { desc = 'Buffers' })
   keymap.set(n, '<leader>fh', cmd 'Telescope help_tags',  { desc = 'Help tags' })
   keymap.set(n, '<leader>fp', cmd 'Telescope projects',   { desc = 'Projects' })
   keymap.set(n, '<leader>fo', cmd 'Telescope oldfiles',   { desc = 'Recently opened files' })

   keymap.set(n, '<leader>fm', cmd 'MarksListBuf', { desc = 'Marks' })

   keymap.set(n, '<C-/>', cmd 'Telescope current_buffer_fuzzy_find', { desc = 'Search in buffer' })
end -- }}}

-- LSP {{{
function M.lspconfig(bufnr)
   which_key.name(n, '<leader>l', 'LSP')

   -- Lspconfig bindings {{{
   -- See `:help vim.lsp.*` for documentation on any of the below functions

   keymap.set(n, 'gd', vim.lsp.buf.definition,     { desc = 'LSP: go to definition',         buffer = bufnr })
   keymap.set(n, 'gD', vim.lsp.buf.declaration,    { desc = 'LSP: go to declaration',        buffer = bufnr })
   keymap.set(n, 'gi', vim.lsp.buf.implementation, { desc = 'LSP: list all implementations', buffer = bufnr })
   -- keymap.set(n, '<F2>',   vim.lsp.buf.definition,  { desc = 'LSP: go to definition',  buffer = bufnr })
   -- keymap.set(n, '<S-F2>', vim.lsp.buf.declaration, { desc = 'LSP: go to declaration', buffer = bufnr })

   keymap.set(n, '<leader>lr', vim.lsp.buf.rename, { desc = 'Rename', buffer = bufnr })

   -- Code action
   -- keymap.set(n, '<leader>la', cmd 'CodeActionMenu', { desc = 'Code action', buffer = bufnr })
   -- keymap.set(n, '<leader>la', vim.lsp.buf.code_action, { desc = 'Code action', buffer = bufnr })

   if vim.bo.filetype ~= 'vim' then
      keymap.set(n, 'K', vim.lsp.buf.hover, { desc = 'LSP: hover doc', buffer = bufnr })
   end

   -- Signature help
   -- WARNING <C-k> conflicts with vim windows and tmux panes navigation.
   -- keymap.set(n, '<C-k>', vim.lsp.buf.signature_help, { buffer = bufnr })

   -- Type definition
   keymap.set(n, '<leader>lt', vim.lsp.buf.type_definition, { desc = 'LSP: type definition', buffer = bufnr })

   -- Find the cursor word definition and reference.
   -- keymap.set(n, 'gr',         vim.lsp.buf.references,  { desc = 'References', buffer = bufnr })
   -- keymap.set(n, '<leader>lf', vim.lsp.buf.references,  { desc = 'References', buffer = bufnr })

   -- keymap.set(n, '<leader>lf', vim.lsp.buf.formatting, { buffer = bufnr })

   keymap.set(n, '<leader>lv', cmd 'Vista nvim_lsp', { desc = 'LSP: show symbols', buffer = bufnr })

   which_key.name(n, '<leader>lw', 'LSP: workspace')
   keymap.set(n, '<leader>lwl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
                                { desc = 'List workspace folders', buffer = bufnr })
   keymap.set(n, '<leader>lwa', vim.lsp.buf.add_workspace_folder,    { desc = 'Add workspace folder', buffer = bufnr })
   keymap.set(n, '<leader>lwr', vim.lsp.buf.remove_workspace_folder, { desc = 'Remove workspace folder', buffer = bufnr })

   --}}}

   -- Lspsaga {{{

   local saga_codeaction = require('lspsaga.codeaction')
   local saga_provider   = require('lspsaga.provider')
   -- local saga_action     = require('lspsaga.action')

   keymap.set(n, '<leader>lf', saga_provider.lsp_finder, { desc = 'LSP finder', buffer = bufnr })
   --
   -- -- Preview definition
   -- keymap.set(n, '<leader>ld', cmd 'Lspsaga preview_definition', { desc = 'Preview definition', buffer = bufnr })

   -- Code action
   keymap.set(n, '<leader>la', saga_codeaction.code_action,    { desc = 'Code action', buffer = bufnr })
   -- keymap.set(n, '<leader>la', cmd 'Lspsaga code_action', { desc = 'Code action', buffer = bufnr })
   -- keymap.set('x', '<leader>la', codeaction.range_code_action,         { desc = 'Range code action', buffer = bufnr })
   keymap.set('x', '<leader>la', cmd '<C-U>Lspsaga range_code_action', { desc = 'Range code action', buffer = bufnr })

   -- -- Hover doc
   -- if vim.bo.filetype ~= 'vim' then
   --    keymap.set(n, 'K', cmd 'Lspsaga hover_doc', { desc = 'LSP: hover doc', buffer = bufnr })
   -- end
   --
   -- -- Show signature help
   -- keymap.set(n, '<C-k>', cmd 'Lspsaga signature_help', { desc = 'Show sinature help', buffer = bufnr })
   --
   -- -- scroll down / up inside different preview windows
   -- keymap.set(n, '<C-u>', function() saga_action.smart_scroll_with_saga(-1, '<c-u>') end, { buffer = bufnr })
   -- keymap.set(n, '<C-d>', function() saga_action.smart_scroll_with_saga( 1, '<c-d>') end, { buffer = bufnr })
   -- -- keymap.set(n, '<C-u>', vim.cmd "lua require('lspsaga.action').smart_scroll_with_saga(-1, '<c-u>')", { buffer = bufnr })
   -- -- keymap.set(n, '<C-d>', vim.cmd "lua require('lspsaga.action').smart_scroll_with_saga(1, '<c-d>')",  { buffer = bufnr })

   --}}}

   -- Diagnostics {{{

   -- keymap.set(n, '<leader>ll', vim.diagnostic.setloclist, { desc = 'Diagnostics in loclist', buffer = bufnr })

   keymap.set(n, '<leader>le', vim.diagnostic.open_float, { desc = 'Show errors',         buffer = bufnr })
   keymap.set(n,    '[e',      vim.diagnostic.goto_prev,  { desc = 'Previous diagnostic', buffer = bufnr })
   keymap.set(n,    ']e',      vim.diagnostic.goto_next,  { desc = 'Next diagnostic',     buffer = bufnr })

   -- -- Lspsaga
   -- keymap.set(n, '<leader>le', cmd 'Lspsaga show_line_diagnostics', { desc = 'Show diagnostic',     buffer = bufnr })
   -- keymap.set(n,    '[e',      cmd 'Lspsaga diagnostic_jump_next',  { desc = 'Previous diagnostic', buffer = bufnr, silent = true })
   -- keymap.set(n,    ']e',      cmd 'Lspsaga diagnostic_jump_prev',  { desc = 'Next diagnostic',     buffer = bufnr, silent = true })

   -- }}}

   -- Renamer {{{

   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lr', cmd 'Lspsaga rename', {silent = true, noremap = true})

   -- keymap.set({n,v}, '<leader>lr', cmd 'lua require("renamer").rename()', { buffer = bufnr })

   -- keymap.set({n,v}, '<leader>lr', cmd 'lua require("renamer").rename()', { buffer = bufnr })
   -- keymap.set({n,v},         'gr', cmd 'lua require("renamer").rename()', { buffer = bufnr })

   -- keymap.set({n,v}, '<leader>lr', require('renamer').rename, { buffer = bufnr })
   -- keymap.set({n,v},         'gr', require('renamer').rename, { buffer = bufnr })

   -- }}}

end --}}}

-- LuaSnip {{{
function M.luasnip()
   local available_luasnip, luasnip = pcall(require, 'luasnip')
   if not available_luasnip then return end

   keymap.set(n, '<Tab>', function() --{{{
      -- if luasnip.expand_or_jumpable() then
      if luasnip.expand_or_locally_jumpable() then
         luasnip.expand_or_jump()
      -- else
      --    vim.api.nvim_feedkeys('<Tab>', 'n', true)
      --    -- vim.api.nvim_command('normal! <Tab>')
      end
   end) --}}}

   keymap.set(n, '<S-Tab>', function() --{{{
      if luasnip.jumpable(-1) then
         luasnip.jump(-1)
      -- else
      --    vim.api.nvim_feedkeys('<S-Tab>', 'n', true)
      end
   end) --}}}

end --}}}

-- Treesitter {{{
function M.treesitter_textobjects()
   return {
      select = { --{{{
         enable = true,
         lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim.
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
   keymap.set(n, '<F3>', cmd 'NvimTreeToggle', {desc = 'Open file-explorer'})

   -- local tree_cb = require'nvim-tree.config'.nvim_tree_callback
   -- vim.g.nvim_tree_bindings = {
   --    { key = '?', cb = tree_cb("toggle_help") }  -- help UI
   -- }

end --}}}

-- nnn file manager {{{
function M.nnn()
   keymap.set(n, '<F4>', cmd 'NnnPicker', {desc = 'Open file-explorer'})
   -- keymap.set(n, '<F3>', cmd 'NnnExplorer', {desc = 'Open file-explorer'})
end --}}}

-- Barbar (tabline) {{{
function M.barbar()
   local opts = { silent = true }

   -- Move to previous/next
   keymap.set(n, '<A-,>', cmd 'BufferPrevious', opts)
   keymap.set(n, '<A-.>', cmd 'BufferNext', opts)
   -- Re-order to previous/next
   keymap.set(n, '<A-<>', cmd 'BufferMovePrevious', opts)
   keymap.set(n, '<A->>', cmd 'BufferMoveNext', opts)
   -- Goto buffer in position...
   keymap.set(n, '<A-1>', cmd 'BufferGoto 1', opts)
   keymap.set(n, '<A-2>', cmd 'BufferGoto 2', opts)
   keymap.set(n, '<A-3>', cmd 'BufferGoto 3', opts)
   keymap.set(n, '<A-4>', cmd 'BufferGoto 4', opts)
   keymap.set(n, '<A-5>', cmd 'BufferGoto 5', opts)
   keymap.set(n, '<A-6>', cmd 'BufferGoto 6', opts)
   keymap.set(n, '<A-7>', cmd 'BufferGoto 7', opts)
   keymap.set(n, '<A-8>', cmd 'BufferGoto 8', opts)
   keymap.set(n, '<A-9>', cmd 'BufferLast', opts)
   -- Close buffer
   keymap.set(n, '<A-c>', cmd 'BufferClose', opts)
   -- Wipeout buffer
   --                          BufferWipeout
   -- Close commands
   --                          BufferCloseAllButCurrent
   --                          BufferCloseBuffersLeft
   --                          BufferCloseBuffersRight
   -- Magic buffer-picking mode
   keymap.set(n, '<C-s>',    cmd 'BufferPick', opts)
   -- Sort automatically by...
   keymap.set(n, '<Space>bd', cmd 'BufferOrderByDirectory', opts)
   keymap.set(n, '<Space>bl', cmd 'BufferOrderByLanguage', opts)

   -- Other:
   -- :BarbarEnable - enables barbar (enabled by default)
   -- :BarbarDisable - very bad command, should never be used

end -- }}}

-- Asterisks {{{
function M.asterisks()
   keymap.set('', '*', '<Plug>(asterisk-z*)')
   keymap.set('', '#', '<Plug>(asterisk-z#)')
   -- keymap.set('', 'g*', '<Plug>(asterisk-gz*)', { desc = ':help gstar' })
   -- keymap.set('', 'g#', '<Plug>(asterisk-gz#)', { desc = ':help g#' })
   keymap.set('', 'g*', '<Plug>(asterisk-gz*)', { desc = 'which_key_ignore' })
   keymap.set('', 'g#', '<Plug>(asterisk-gz#)', { desc = 'which_key_ignore' })
end -- }}}

return M

-- vim: fdm=marker
