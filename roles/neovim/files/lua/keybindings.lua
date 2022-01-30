--  ██                           ██      ██              ██ ██
-- ░██                          ░██     ░░              ░██░░
-- ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
-- ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
-- ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
-- ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
-- ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
-- ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
--                  ░░░░░                                              ░░░░░

local keymap = require "util".keymap
local M = {}

local n, v = 'n', 'v'
local function cmd(command) return table.concat({'<cmd>', command, '<CR>'}) end

-- Dealing with word wrap:
-- If cursor is inside very long line in the file than wraps around several rows
-- on the screen, then 'j' key moves you to the next line in the file, but not
-- to the next row on the screen under your previous position as in other
-- editors.  These bindings fixes this.
keymap.set(n, 'k', function() return vim.v.count > 0 and 'k' or 'gk' end, {silent = true, expr = true})
keymap.set(n, 'j', function() return vim.v.count > 0 and 'j' or 'gj' end, {silent = true, expr = true})

-- Hop (Easymotion) {{{
function M.hop()
   keymap.set({n,v}, ';w', function() vim.cmd('HopWordAC') end, {desc = 'Easymotion forward word'})
   keymap.set({n,v}, ';b', function() vim.cmd('HopWordBC') end, {desc = 'Easymotion bacward word'})

   keymap.set({n,v}, ';j', cmd 'HopLineAC', {desc = 'Easymotion line up'})
   keymap.set({n,v}, ';k', cmd 'HopLineBC', {desc = 'Easymotion line down'})

   keymap.set({n,v}, 's', cmd 'HopChar1', {desc = 'Easymotion char'})

   keymap.set(n, 't', cmd 'HopChar2', {desc = 'Easymotion 2 chars'})
end -- }}}

-- Telescope {{{
function M.telescope()
   keymap.set(n, '<C-;>', cmd 'Telescope commands',        {desc = 'Execute command'})
   keymap.set(n, 'q:',    cmd 'Telescope command_history', {desc = 'Command-line history'})
   keymap.set(n, 'q/',    cmd 'Telescope search_history',  {desc = 'Search history'})
   keymap.set(n, 'q?',    cmd 'Telescope search_history',  {desc = 'Search history'})

   keymap.set(n, 'z=',    cmd 'Telescope spell_suggest',   {desc = 'Spell Suggest'})

   which_key.name(n, '<leader>f', 'Telescope')

   keymap.set(n, '<leader>ff', cmd 'Telescope find_files', {desc = 'Find files'})
   keymap.set(n, '<leader>fg', cmd 'Telescope live_grep',  {desc = 'Live grep'})
   keymap.set(n, '<leader>fb', cmd 'Telescope buffers',    {desc = 'Buffers'})
   keymap.set(n, '<leader>fh', cmd 'Telescope help_tags',  {desc = 'Help tags'})
   keymap.set(n, '<leader>fp', cmd 'Telescope projects',   {desc = 'Projects'})
   keymap.set(n, '<leader>fo', cmd 'Telescope vim_options',{desc = 'Vim Options'})

   keymap.set(n, '<leader>fm', cmd 'MarksListBuf', {desc = 'Marks'})

   keymap.set(n, '<C-/>', cmd 'Telescope current_buffer_fuzzy_find', {desc = 'Search in buffer'})
end -- }}}

-- LSP {{{
function M.lspconfig(bufnr)
   which_key.name(n, '<leader>l', 'LSP')

   -- Lspconfig bindings {{{
   -- See `:help vim.lsp.*` for documentation on any of the below functions

   keymap.set(n, '<F2>',   vim.lsp.buf.definition,  {desc = 'LSP: go to definition', buffer = bufnr})
   keymap.set(n, '<S-F2>', vim.lsp.buf.declaration, {desc = 'LSP: go to declaration', buffer = bufnr})

   if vim.bo.filetype ~= 'vim' then
      keymap.set(n, 'K', vim.lsp.buf.hover, {desc = 'LSP: Hover doc', buffer = bufnr})
   end

   -- keymap.set(n, 'gi',         vim.lsp.buf.implementation,          { buffer = bufnr })
   -- keymap.set(n, '<C-k>',      vim.lsp.buf.signature_help,          { buffer = bufnr })
   -- keymap.set(n, '<leader>wa', vim.lsp.buf.add_workspace_folder,    { buffer = bufnr })
   -- keymap.set(n, '<leader>wr', vim.lsp.buf.remove_workspace_folder, { buffer = bufnr })
   -- keymap.set(n, '<leader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, { buffer = bufnr })

   keymap.set(n, '<leader>lt', vim.lsp.buf.type_definition, {desc = 'LSP: Type definition', buffer = bufnr})

   -- keymap.set(n, '<leader>rn', vim.lsp.buf.rename,      { buffer = bufnr })
   -- keymap.set(n, '<leader>ca', vim.lsp.buf.code_action, { buffer = bufnr })
   -- keymap.set(n, 'gr',         vim.lsp.buf.references,  { buffer = bufnr })

   keymap.set(n, '<leader>le', vim.lsp.diagnostic.show_line_diagnostics, {desc = 'Show errors', buffer = bufnr})

   -- keymap.set(n, '[d',        vim.lsp.diagnostic.goto_prev,   { buffer = bufnr })
   -- keymap.set(n, ']d',        vim.lsp.diagnostic.goto_next,   { buffer = bufnr })
   -- keymap.set(n, '<leader>q', vim.lsp.diagnostic.set_loclist, { buffer = bufnr })
   -- keymap.set(n, '<leader>f', vim.lsp.buf.formatting,         { buffer = bufnr })

   --}}}

   -- Lspsaga {{{

   keymap.set(n, '<leader>le', cmd('Lspsaga lsp_finder'), {desc = 'Show line diagnostics', buffer = bufnr})

   -- wk.register({ ['<leader>c'] = {name = 'LSP: Code action'}  }, {mode = 'n'})
   -- wk.register({ ['<leader>c'] = {name = 'LSP: Code action'}  }, {mode = 'v'})

   keymap.set(n, '<leader>la', cmd 'Lspsaga code_action',            {desc = 'Code action',            buffer = bufnr})
   keymap.set(v, '<leader>la', cmd '<C-U>Lspsaga range_code_action', {desc = 'LSP: Range code action', buffer = bufnr})

   -- rename
   -- keymap.set('n',         'gr', cmd 'Lspsaga rename', {desc = 'Rename', buffer = bufnr})
   -- keymap.set('n', '<leader>lr', cmd 'Lspsaga rename', {desc = 'Rename', buffer = bufnr})

   -- preview definition
   keymap.set(n, '<leader>ld', cmd 'Lspsaga preview_definition', {desc = 'Preview definition', buffer = bufnr})

   -- -- hover doc
   -- buf_set_keymap(bufnr, 'n', 'K', 'Hover doc', cmd 'Lspsaga hover_doc')

   -- show signature help
   keymap.set(n, '<C-k>', cmd 'Lspsaga signature_help', {desc = 'LSP: Show sinature help', buffer = bufnr})

   -- Show Diagnostics
   keymap.set(n, '<leader>le', cmd 'Lspsaga show_line_diagnostics', {desc = 'Show diagnostic', buffer = bufnr})
   -- only show diagnostic if cursor is over the area
   -- buf_set_keymap(bufnr, 'n', '<leader>le', 'Show diagnostic', require'lspsaga.diagnostic'.show_cursor_diagnostics)

   -- -- jump diagnostic
   -- buf_set_keymap(bufnr, 'n', ']e', cmd 'Lspsaga diagnostic_jump_next')
   -- buf_set_keymap(bufnr, 'n', '[e', cmd 'Lspsaga diagnostic_jump_prev')


   -- -- scroll down / up inside different preview windows
   -- buf_set_keymap(bufnr, 'n', '<C-f>', cmd "lua require('lspsaga.action').smart_scroll_with_saga(1)")
   -- buf_set_keymap(bufnr, 'n', '<C-b>', cmd "lua require('lspsaga.action').smart_scroll_with_saga(-1)")

   --}}}

   -- Renamer {{{

   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lr', cmd 'Lspsaga rename', {silent = true, noremap = true})

   keymap.set({n,v}, '<leader>lr', cmd 'lua require("renamer").rename()', { buffer = bufnr })

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
   -- keymap.set('', 'g*', '<Plug>(asterisk-gz*)', {desc = ':help gstar'})
   -- keymap.set('', 'g#', '<Plug>(asterisk-gz#)', {desc = ':help g#'})
   keymap.set('', 'g*', '<Plug>(asterisk-gz*)', {desc = 'which_key_ignore'})
   keymap.set('', 'g#', '<Plug>(asterisk-gz#)', {desc = 'which_key_ignore'})
end -- }}}

return M

-- vim: fdm=marker
