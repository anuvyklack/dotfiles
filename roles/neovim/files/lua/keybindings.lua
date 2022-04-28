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
local keymap = require('util').keymap
local which_key = require('util').which_key
local n, v, x = 'n', 'v', 'x'
local function cmd(command) return table.concat({ '<cmd>', command, '<CR>' }) end

-- -- Dealing with word wrap:
-- -- If cursor is inside very long line in the file than wraps around several rows
-- -- on the screen, then 'j' key moves you to the next line in the file, but not
-- -- to the next row on the screen under your previous position as in other
-- -- editors. These bindings fixes this.
-- keymap.set(n, 'k', function() return vim.v.count > 0 and 'k' or 'gk' end, { silent = true, expr = true })
-- keymap.set(n, 'j', function() return vim.v.count > 0 and 'j' or 'gj' end, { silent = true, expr = true })

-- Hop (Easymotion) {{{
function M.hop()
   keymap.set({n,v}, ';w', function() vim.cmd('HopWordAC') end, { desc = 'Easymotion forward word' })
   keymap.set({n,v}, ';b', function() vim.cmd('HopWordBC') end, { desc = 'Easymotion bacward word' })

   keymap.set({n,v}, ';j', cmd 'HopLineAC', { desc = 'Easymotion line up' })
   keymap.set({n,v}, ';k', cmd 'HopLineBC', { desc = 'Easymotion line down' })

   keymap.set({n,v}, 's', cmd 'HopChar1', { desc = 'Easymotion char' })

   keymap.set(n, 't', cmd 'HopChar2', { desc = 'Easymotion 2 chars' })
end -- }}}

-- Fuzzy finders and pickers (telescope, fzf, etc) {{{

-- Telescope {{{
function M.telescope()
   keymap.set(n, '<C-;>', cmd 'Telescope commands',        { desc = 'Execute command',      requires = 'telescope' })
   keymap.set(n, 'q:',    cmd 'Telescope command_history', { desc = 'Command-line history', requires = 'telescope' })
   keymap.set(n, 'q/',    cmd 'Telescope search_history',  { desc = 'Search history',       requires = 'telescope' })
   keymap.set(n, 'q?',    cmd 'Telescope search_history',  { desc = 'Search history',       requires = 'telescope' })

   keymap.set(n, 'z=',    cmd 'Telescope spell_suggest',   { desc = 'Spell Suggest', requires = 'telescope' })

   which_key.name(n, '<leader>f', 'Telescope')

   keymap.set(n, '<leader>fa', cmd 'Telescope builtin',    { desc = 'List all pickers', requires = 'telescope' })
   keymap.set(n, '<leader>ff', cmd 'Telescope find_files', { desc = 'Find files',       requires = 'telescope' })
   keymap.set(n, '<leader>fg', cmd 'Telescope live_grep',  { desc = 'Live grep',        requires = 'telescope' })
   -- keymap.set(n, '<leader>fb', cmd 'Telescope buffers',    { desc = 'Buffers',   requires = 'telescope' })
   keymap.set(n, '<leader>fh', cmd 'Telescope help_tags',  { desc = 'Help tags', requires = 'telescope' })
   keymap.set(n, '<leader>fp', cmd 'Telescope projects',   { desc = 'Projects',  requires = 'telescope' })
   keymap.set(n, '<leader>fo', cmd 'Telescope oldfiles',   { desc = 'Recently opened files', requires = 'telescope' })

   keymap.set(n, '<C-/>', cmd 'Telescope current_buffer_fuzzy_find', { desc = 'Search in buffer', requires = 'telescope' })
end --}}}

keymap.set(n, '<leader>fm', cmd 'MarksListBuf', { desc = 'Marks', requires = 'marks' })

-- }}}

-- LSP {{{
function M.lspconfig(bufnr)
   -- See `:help vim.lsp.*` for documentation functions

   local opts = setmetatable({ --{{{
      buffer = bufnr
   }, {
      __call = function (self, opt)
         for key, value in pairs(opt) do
            self[key] = value
         end
         return self
      end
   }) --}}}

   which_key.name(n, '<leader>l', 'LSP')

   keymap.set(n, '<leader>lv', cmd 'Vista nvim_lsp', opts{ desc = 'LSP: show symbols' })

   -- Go to definiton, declaration, references, etc {{{

   keymap.set(n, 'gd', vim.lsp.buf.definition,     opts{ desc = 'LSP: go to definition' })
   keymap.set(n, 'gD', vim.lsp.buf.declaration,    opts{ desc = 'LSP: go to declaration' })
   keymap.set(n, 'gi', vim.lsp.buf.implementation, opts{ desc = 'LSP: list all implementations' })
   -- keymap.set(n, '<F2>',   vim.lsp.buf.definition,  opts{ desc = 'LSP: go to definition' })
   -- keymap.set(n, '<S-F2>', vim.lsp.buf.declaration, opts{ desc = 'LSP: go to declaration' })


   -- Find the cursor word definition and reference.
   keymap.set(n, 'gr', vim.lsp.buf.references,                  opts { desc = 'LSP References' })
   keymap.set(n, 'gR', '<Cmd>TroubleToggle lsp_references<CR>', opts { desc = 'LSP references', requires = 'trouble' })
   -- keymap.set(n, '<leader>lr', vim.lsp.buf.references, opts{ desc = 'References' })

   -- }}}

   -- Renamer {{{

   keymap.set(n, '<leader>lr', vim.lsp.buf.rename, opts{ desc = 'Rename' })

   -- keymap.set({n,v}, '<leader>lr', require('renamer').rename, opts{ desc = 'Rename', requires = 'renamer' })
   -- keymap.set({n,v},         'gr', require('renamer').rename, opts{ desc = 'Rename', requires = 'renamer' })

   -- }}}

   -- Code action {{{

   keymap.set(n, '<leader>la', vim.lsp.buf.code_action, opts{ desc = 'Code action' })

   -- keymap.set(n, '<leader>la', cmd 'CodeActionMenu',
   --                             opts{ desc = 'Code action', requires = 'code_action_menu' })

   -- }}}

   -- Diagnostics {{{

   -- keymap.set(n, '<leader>ll', vim.diagnostic.setloclist, opts{ desc = 'Diagnostics in loclist' })

   keymap.set(n, '<leader>le', vim.diagnostic.open_float, opts{ desc = 'Show errors' })
   keymap.set(n,    '[e',      vim.diagnostic.goto_prev,  opts{ desc = 'Previous diagnostic' })
   keymap.set(n,    ']e',      vim.diagnostic.goto_next,  opts{ desc = 'Next diagnostic' })

   -- }}}

   -- Hover doc
   keymap.set(n, 'K', vim.lsp.buf.hover, opts{ desc = 'LSP: hover doc', ft_ignore = { 'vim' } })

   -- Signature help
   -- <C-k> conflicts with vim windows and tmux panes navigation.
   keymap.set(n, '<leader>ls', vim.lsp.buf.signature_help, opts{ desc = 'Show sinature help' })

   -- Type definition
   keymap.set(n, '<leader>lt', vim.lsp.buf.type_definition, opts{ desc = 'LSP: type definition' })

   -- Formatting
   keymap.set(n, '<leader>lf', vim.lsp.buf.formatting, opts)

   -- Workspace {{{
   which_key.name(n, '<leader>lw', 'LSP: workspace')

   keymap.set(n, '<leader>lwl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
                                opts{ desc = 'List workspace folders' })
   keymap.set(n, '<leader>lwa', vim.lsp.buf.add_workspace_folder,    opts{ desc = 'Add workspace folder' })
   keymap.set(n, '<leader>lwr', vim.lsp.buf.remove_workspace_folder, opts{ desc = 'Remove workspace folder' })

   --}}}

end --}}}

-- Trouble {{{
function M.trouble()
   keymap.set(n, '<leader>ee', '<cmd>TroubleToggle<cr>',                       { desc = 'Trouble: toggle win' })
   keymap.set(n, '<leader>ew', '<cmd>TroubleToggle workspace_diagnostics<cr>', { desc = 'workspace diagnostics' })
   keymap.set(n, '<leader>ed', '<cmd>TroubleToggle document_diagnostics<cr>',  { desc = 'buffer diagnostics' })
   keymap.set(n, '<leader>eq', '<cmd>TroubleToggle quickfix<cr>',              { desc = 'Trouble: show quickfix' })
   keymap.set(n, '<leader>el', '<cmd>TroubleToggle loclist<cr>',               { desc = 'Trouble: show loclist' })
end -- }}}

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

-- Gitsigns {{{
function M.gitsigns(bufnr)
   local gs = package.loaded.gitsigns

   local function map(mode, lhs, rhs, opts) --{{{
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, lhs, rhs, opts)
   end --}}}

   -- Navigation
   map('n', ']c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(function() gs.next_hunk() end)
      return '<Ignore>'
   end, { expr = true, desc = 'Git: next hunk' })

   map('n', '[c', function()
      if vim.wo.diff then return '[c' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
   end, { expr = true, desc = 'Git: prev hunk' })

   -- Actions
   map({n,v}, '<leader>hs', '<Cmd>Gitsigns stage_hunk<CR>', { desc = 'Git: stage hunk' })
   map({n,v}, '<leader>hr', '<Cmd>Gitsigns reset_hunk<CR>', { desc = 'Git: reset hunk' })
   map(n, '<leader>hS', gs.stage_buffer, { desc = 'Git: stage buffer' })
   map(n, '<leader>hu', gs.undo_stage_hunk)
   map(n, '<leader>hR', gs.reset_buffer)
   map(n, '<leader>hp', gs.preview_hunk, { desc = 'Git: preview buffer' })
   map(n, '<leader>hb', function() gs.blame_line{ full=true } end)
   map(n, '<leader>tb', gs.toggle_current_line_blame, { desc = 'Git: line blame' })
   map(n, '<leader>hd', gs.diffthis)
   map(n, '<leader>hD', function() gs.diffthis('~') end)
   map(n, '<leader>td', gs.toggle_deleted, { desc = 'Git: toggle deleted' })

   -- Text object
   map({'o','x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>', { desc = 'Git: inner hunk' })
end
-- }}}

-- File managment {{{
function M.nvim_tree()
   -- NvimTreeToggle
   -- NvimTreeRefresh
   -- NvimTreeFindFile
   -- NvimTreeOpen
   -- NvimTreeClose
   -- NvimTreeFocus
   -- NvimTreeFindFileToggle
   -- NvimTreeResize
   -- NvimTreeCollapse
   -- NvimTreeCollapseKeepBuffers
   keymap.set(n, '<F3>', cmd 'NvimTreeToggle', { desc = 'Open file-tree', requires = 'nvim-tree' })
end

function M.neo_tree()
   -- keymap.set('n', [[\]], '<cmd>Neotree reveal<cr>', { desc = 'Open file-explorer', requires = 'neo-tree' })
   keymap.set('n', '<F3>', '<cmd>Neotree toggle reveal<cr>', { desc = 'Open file-tree', requires = 'neo-tree' })
end

-- nnn (file manager)
function M.nnn()
   keymap.set(n, '<F4>', cmd 'NnnPicker', { desc = 'Open file-tree' })
   -- keymap.set(n, '<F3>', cmd 'NnnExplorer', { desc = 'Open file-explorer' })
end
--}}}

-- Barbar (tabline) {{{
function M.barbar()

   local opt = setmetatable({ --{{{
      silent = true
   }, {
      __call = function (self, opt)
         for key, value in pairs(opt) do
            self[key] = value
         end
         return self
      end
   }) --}}}

   -- Keys with '<', '>': move to previous/next
   keymap.set(n, '<A-,>', cmd 'BufferPrevious', opt)
   keymap.set(n, '<A-.>', cmd 'BufferNext', opt)

   -- Re-order to previous/next
   keymap.set(n, '<A-<>', cmd 'BufferMovePrevious', opt)
   keymap.set(n, '<A->>', cmd 'BufferMoveNext', opt)

   -- Close buffer
   keymap.set(n, '<A-c>', cmd 'BufferClose', opt)

   -- Wipeout buffer
   --                          BufferWipeout
   -- Close commands
   --                          BufferCloseAllButCurrent
   --                          BufferCloseBuffersLeft
   --                          BufferCloseBuffersRight

   -- Magic buffer-picking mode
   keymap.set(n, '<C-s>',    cmd 'BufferPick', opt)

   -- Sort automatically by...
   keymap.set(n, '<Space>bd', cmd 'BufferOrderByDirectory', opt)
   keymap.set(n, '<Space>bl', cmd 'BufferOrderByLanguage', opt)

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

-- vim: fml=1 fdm=marker
