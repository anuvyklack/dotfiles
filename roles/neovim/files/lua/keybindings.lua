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
local util = require('util')
local prequire = util.prequire
local Hydra = prequire("hydra")
local keymap = util.keymap
keymap.amend = prequire('keymap-amend')
local cmd = keymap.cmd
local which_key = util.which_key
local telescope_pickers = require('anuvyklack/telescope/pickers')

M.lsp = function(bufnr)
   local opts = setmetatable({ buffer = bufnr },{
      __call = function(self, input)
         local opts = vim.deepcopy(self)
         for key, value in pairs(input) do
            opts[key] = value
         end
         return opts
      end
   })

   keymap.set('n', 'gd', vim.lsp.buf.definition,     opts{ desc = 'LSP go to definition' })
   keymap.set('n', 'gD', vim.lsp.buf.declaration,    opts{ desc = 'LSP go to declaration' })
   keymap.set('n', 'gi', vim.lsp.buf.implementation, opts{ desc = 'LSP list implementations' })
   -- keymap.set('n', 'gd', require('goto-preview').goto_preview_definition)
   -- keymap.set('n', 'gi', require('goto-preview').goto_preview_implementation)
   keymap.set('n', 'gr', vim.lsp.buf.references,     opts{ desc = 'LSP references' })
   -- keymap.set('n', 'gR', cmd 'TroubleToggle lsp_references', opts { desc = 'LSP references', requires = 'trouble' })

   keymap.set('n', 'K', vim.lsp.buf.hover, opts{ desc = 'hover doc', ft_ignore = { 'vim' } })

   which_key.name('n', '<leader>l', 'LSP')

   local hint = [[
 _r_ rename
 _a_ code action
 _s_ signature help
 _t_ type definition
 _f_ format
 _v_ Vista
]]

   Hydra {
      name = 'LSP',
      hint = hint,
      config = {
         color = 'teal',
         buffer = bufnr,
         invoke_on_body = true,
         hint = {
            -- position = 'middle-right',
            border = 'rounded'
         }
      },
      mode = {'n','x'},
      body = '<leader>l',
      heads = {
         { 'r', vim.lsp.buf.rename, { desc = 'rename' } },
         -- { 'r', prequire('renamer').rename, { desc = 'rename' } },

         { 'a', vim.lsp.buf.code_action, { desc = 'code action' } },

         { 's', vim.lsp.buf.signature_help,  { desc = 'sinature help' } },
         { 't', vim.lsp.buf.type_definition, { desc = 'type definition' } },
         { 'f', vim.lsp.buf.formatting,      { desc = 'format' } },

         { 'te', telescope_pickers.buffer_diagnostics,    { desc = 'buffer diagnostics' } },
         { 'tE', telescope_pickers.workspace_diagnostics, { desc = 'workspace diagnostics' } },

         { 'td', telescope_pickers.definitions,       { desc = 'definitions' } },
         { 'tr', telescope_pickers.buffer_references, { desc = 'buffer references' } },
         { 'tR', telescope_pickers.references,        { desc = 'references' } },

         { 'ti', telescope_pickers.implementations, { desc = 'implementations' } },

         { 'ts', telescope_pickers.document_symbols,  { desc = 'symbols' } },
         { 'tS', telescope_pickers.workspace_symbols, { desc = 'workspace symbols' } },

         { 'v', cmd 'Vista nvim_lsp', { desc = 'Vista' } },
         { '<Esc>', nil, { exit = true } }
      }
   }

   -- -- Workspace
   -- which_key.name('n', '<leader>lw', 'workspace')
   -- keymap.set('n', '<leader>lwl',
   --    function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
   --    opts{ desc = 'list workspace folders' })
   -- keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder,    opts{ desc = 'add workspace folder' })
   -- keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, opts{ desc = 'remove workspace folder' })

end

M.telescope = function()

   local hint; do

   -- hint = [[
--  ^ ^             ^ ^             🭇🬭🬭🬭🬭🬭🬭🬭🬭🬼
--  ^ ^             ^ ^            🭉🭁🭠🭘    🭣🭕🭌🬾
--  ^ ^             ^ ^            🭅█ ▁     █🭐
--  ^ ^             ^ ^            ██🬿      🭊██
--  ^ ^             ^ ^           🭋█🬝🮄🮄🮄🮄🮄🮄🮄🮄🬆█🭀
--  ^ ^             ^ ^           🭤🭒🬺🬹🬱🬭🬭🬭🬭🬵🬹🬹🭝🭙
--  ^
--  _f_: files       _m_: marks            _h_: vim help   _c_: execute command
--  _o_: old files   _g_: live grep        _k_: keymap     _;_: commands history
--  _p_: projects    _/_: search in file   _r_: registers  _?_: search history
--  ^
--  ^ ^              ^ ^        _<Enter>_: Telescope       ^ ^            _<Esc>_
-- ]]

   -- hint = [[
--    🭇🬭🬭🬭🬭🬭🬭🬭🬭🬼
--   🭉🭁🭠🭘    🭣🭕🭌🬾  _f_: files       _m_: marks            _h_: vim help   _c_: execute command
--   🭅█ ▁     █🭐  _o_: old files   _g_: live grep        _k_: keymap     _;_: commands history
--   ██🬿      🭊██  _p_: projects    _/_: search in file   _r_: registers  _?_: search history
--  🭋█🬝🮄🮄🮄🮄🮄🮄🮄🮄🬆█🭀 ^
--  🭤🭒🬺🬹🬱🬭🬭🬭🬭🬵🬹🬹🭝🭙 ^ ^              ^ ^        _<Enter>_: Telescope       ^ ^            _<Esc>_
-- ]]

   hint = [[
                 _f_: files       _m_: marks
   🭇🬭🬭🬭🬭🬭🬭🬭🬭🬼    _o_: old files   _g_: live grep
  🭉🭁🭠🭘    🭣🭕🭌🬾   _p_: projects    _/_: search in file
  🭅█ ▁     █🭐
  ██🬿      🭊██   _r_: resume      _u_: undotree
 🭋█🬝🮄🮄🮄🮄🮄🮄🮄🮄🬆█🭀  _h_: vim help    _c_: execute command
 🭤🭒🬺🬹🬱🬭🬭🬭🬭🬵🬹🬹🭝🭙  _k_: keymaps     _;_: commands history 
                 _O_: options     _?_: search history
 ^
                 _<Enter>_: Telescope           _<Esc>_
]]

   end

   Hydra({
      name = 'Telescope',
      hint = hint,
      config = {
         color = 'teal',
         invoke_on_body = true,
         hint = {
            position = 'middle',
            border = 'rounded',
         },
      },
      mode = 'n',
      body = '<Leader>f',
      heads = {
         { 'f', cmd 'Telescope find_files' },

         { 'g', cmd 'Telescope live_grep' },
         -- { 'g', telescope_pickers.grep },

         { 'o', cmd 'Telescope oldfiles', { desc = 'recently opened files' } },
         { 'h', cmd 'Telescope help_tags', { desc = 'vim help' } },
         { 'm', cmd 'MarksListBuf', { desc = 'marks' } },
         { 'k', cmd 'Telescope keymaps' },
         { 'O', cmd 'Telescope vim_options' },
         -- { 'r', cmd 'Telescope registers' },
         { 'r', cmd 'Telescope resume' },

         -- { 'p', telescope.extensions.projects.projects },
         { 'p', cmd 'Telescope projects', { desc = 'projects' } },

         { '/', cmd 'Telescope current_buffer_fuzzy_find', { desc = 'search in file' } },
         { '?', cmd 'Telescope search_history',  { desc = 'search history' } },

         { ';', cmd 'Telescope command_history', { desc = 'command-line history' } },
         { 'c', cmd 'Telescope commands', { desc = 'execute command' } },

         { 'u', cmd 'silent! %foldopen! | UndotreeToggle', { desc = 'undotree' }},

         -- { 'j', ':lua require"utils.telescope".jump()<CR>' },
         -- { 'l', telescope.extensions.neoclip.default },
         -- { 'z', telescope.extensions.zoxide.list },

         { '<Enter>', cmd 'Telescope', { exit = true, desc = 'list all pickers' } },
         { '<Esc>', nil, { exit = true, nowait = true } },
      }
   })

   keymap.set('n', 'z=', cmd 'Telescope spell_suggest', { desc = 'spell Suggest' })
end

M.gitsigns = function(bufnr)
   local gitsigns = prequire('gitsigns')

   local hint = [[
 _J_: next hunk   _s_: stage hunk        _d_: show deleted   _b_: blame line
 _K_: prev hunk   _u_: undo last stage   _p_: preview hunk   _B_: blame show full 
 ^ ^              _S_: stage buffer      ^ ^                 _/_: show base file
 ^
 ^ ^              _<Enter>_: Neogit              _q_: exit
]]

   -- Hydra({
   --    name = 'Git',
   --    hint = hint,
   --    config = {
   --       -- debug = true,
   --       buffer = bufnr,
   --       color = 'amaranth',
   --       invoke_on_body = true,
   --       hint = {
   --          border = 'rounded'
   --       },
   --       on_key = function() vim.wait(50) end,
   --       on_enter = function()
   --          vim.cmd('silent! %foldopen!')
   --          gitsigns.toggle_signs(true)
   --          gitsigns.toggle_linehl(true)
   --       end,
   --       on_exit = function()
   --          gitsigns.toggle_signs(false)
   --          gitsigns.toggle_linehl(false)
   --          gitsigns.toggle_deleted(false)
   --       end,
   --    },
   --    mode = {'n','x'},
   --    body = '<leader>g',
   --    heads = {
   --       { 'J',
   --          function()
   --             if vim.wo.diff then return ']c' end
   --             vim.schedule(function() gitsigns.next_hunk() end)
   --             return '<Ignore>'
   --          end,
   --          { expr = true, desc = 'next hunk' } },
   --       { 'K',
   --          function()
   --             if vim.wo.diff then return '[c' end
   --             vim.schedule(function() gitsigns.prev_hunk() end)
   --             return '<Ignore>'
   --          end,
   --          { expr = true, desc = 'prev hunk' } },
   --       { 's',
   --          function()
   --             local mode = vim.api.nvim_get_mode().mode:sub(1,1)
   --             if mode == 'V' then -- visual-line mode
   --                local esc = vim.api.nvim_replace_termcodes('<Esc>', true, true, true)
   --                vim.api.nvim_feedkeys(esc, 'x', false) -- exit visual mode
   --                vim.cmd("'<,'>Gitsigns stage_hunk")
   --             else
   --                vim.cmd("Gitsigns stage_hunk")
   --             end
   --          end,
   --          { desc = 'stage hunk' } },
   --       -- { 'r', ':Gitsigns reset_hunk<CR>', { desc = 'reset hunk' } }, -- need modifiable
   --       { 'u', gitsigns.undo_stage_hunk, { desc = 'undo last stage' } },
   --       { 'S', gitsigns.stage_buffer, { desc = 'stage buffer' } },
   --       -- { 'v', gitsigns.select_hunk, { nowait = true, desc = 'select hunk' } },
   --       { 'p', gitsigns.preview_hunk, { desc = 'preview hunk' } },
   --
   --       { 'd',
   --          function()
   --             gitsigns.toggle_deleted()
   --             -- vim.wait(50, function() vim.cmd 'redraw' end, 10, false)
   --          end,
   --          { nowait = true, desc = 'toggle deleted' } },
   --
   --       -- { 's', telescope_pickers.git_status, { desc = 'status' } },
   --       -- { 'S', telescope_pickers.git_stash, { desc = 'stash' } },
   --
   --       { 'b', gitsigns.blame_line, { desc = 'blame' } },
   --       { 'B', function() gitsigns.blame_line{ full = true } end, { desc = 'blame show full' } },
   --
   --       { '/', gitsigns.show, { exit = true, desc = 'show base file' } }, -- show the base of the file
   --
   --       { '<Enter>', function() vim.cmd('Neogit') end, { exit = true, desc = 'Neogit' } },
   --
   --       { 'q', nil, { exit = true, nowait = true, desc = 'exit' } },
   --       -- { '<Esc>', nil, { exit = true, desc = 'exit' } }
   --    }
   -- })

   Hydra({
      name = 'Git',
      hint = hint,
      config = {
         -- debug = true,
         buffer = bufnr,
         color = 'pink',
         invoke_on_body = true,
         hint = {
            border = 'rounded'
         },
         on_enter = function()
            vim.cmd('silent! %foldopen!')
            vim.bo.modifiable = false
            gitsigns.toggle_signs(true)
            gitsigns.toggle_linehl(true)
            vim.wait(50, function() vim.cmd 'redraw' end, 10, false)
         end,
         on_exit = function()
            gitsigns.toggle_signs(false)
            gitsigns.toggle_linehl(false)
            gitsigns.toggle_deleted(false)
         end,
      },
      mode = {'n','x'},
      body = '<leader>g',
      heads = {
         { 'J',
            function()
               if vim.wo.diff then return ']c' end
               vim.schedule(function() gitsigns.next_hunk() end)
               return '<Ignore>'
            end,
            { expr = true, desc = 'next hunk' } },
         { 'K',
            function()
               if vim.wo.diff then return '[c' end
               vim.schedule(function() gitsigns.prev_hunk() end)
               return '<Ignore>'
            end,
            { expr = true, desc = 'prev hunk' } },
         { 's', ':Gitsigns stage_hunk<CR>', { silent = true, desc = 'stage hunk' } },
         -- { 'r', ':Gitsigns reset_hunk<CR>', { desc = 'reset hunk' } }, -- need modifiable
         { 'u', gitsigns.undo_stage_hunk, { desc = 'undo last stage' } },
         { 'S', gitsigns.stage_buffer, { desc = 'stage buffer' } },
         -- { 'v', gitsigns.select_hunk, { nowait = true, desc = 'select hunk' } },
         { 'p', gitsigns.preview_hunk, { desc = 'preview hunk' } },
         { 'd', gitsigns.toggle_deleted, { nowait = true, desc = 'toggle deleted' } },

         -- { 's', telescope_pickers.git_status, { desc = 'status' } },
         -- { 'S', telescope_pickers.git_stash, { desc = 'stash' } },

         { 'b', gitsigns.blame_line, { desc = 'blame' } },
         { 'B', function() gitsigns.blame_line{ full = true } end, { desc = 'blame show full' } },

         { '/', gitsigns.show, { exit = true, desc = 'show base file' } }, -- show the base of the file

         { '<Enter>', cmd 'Neogit', { exit = true, desc = 'Neogit' } },

         { 'q', nil, { exit = true, nowait = true, desc = 'exit' } },
         -- { '<Esc>', nil, { exit = true, desc = 'exit' } }
      }
   })

end

M.hop = function()
   keymap.set({'n','x'}, ';w', cmd 'HopWordAC', { desc = 'Easymotion forward word' })
   keymap.set({'n','x'}, ';b', cmd 'HopWordBC', { desc = 'Easymotion bacward word' })

   keymap.set({'n','x'}, ';j', cmd 'HopLineAC', { desc = 'Easymotion line up' })
   keymap.set({'n','x'}, ';k', cmd 'HopLineBC', { desc = 'Easymotion line down' })

   keymap.set({'n','x'}, 's', cmd 'HopChar1', { desc = 'Easymotion char' })

   keymap.set('n', 't', cmd 'HopChar2', { desc = 'Easymotion 2 chars' })
end

M.easy_align = function()
   -- n : interactive EasyAlign for a motion/text object (e.g. gaip)
   -- x : interactive EasyAlign in visual mode (e.g. vipga)
   keymap.set({'n','x'}, '<leader>a', '<Plug>(EasyAlign)')
end

M.iswap = function()
   keymap.set({'n','x'}, 'gs', cmd 'ISwap')
end

M.luasnip = function()
   local luasnip = prequire('luasnip')

   keymap.amend('n', '<Tab>', function(original)
      -- if luasnip.expand_or_jumpable() then
      if luasnip.expand_or_locally_jumpable() then
         luasnip.expand_or_jump()
      else
         original()
      end
   end)

   keymap.amend('n', '<S-Tab>', function(original)
      if luasnip.jumpable(-1) then
         luasnip.jump(-1)
      else
         original()
      end
   end)

   keymap.amend({'i','s'}, '<C-n>', function(original)
      if luasnip.choice_active() then
         luasnip.change_choice(1)
      else
         original()
      end
   end)

   keymap.amend({'i','s'}, '<C-p>', function(original)
      if luasnip.choice_active() then
         luasnip.change_choice(-1)
      else
         original()
      end
   end)

end

M.asterisks = function()
   keymap.set('', '*', '<Plug>(asterisk-z*)')
   keymap.set('', '#', '<Plug>(asterisk-z#)')
   -- keymap.set('', 'g*', '<Plug>(asterisk-gz*)', { desc = ':help gstar' })
   -- keymap.set('', 'g#', '<Plug>(asterisk-gz#)', { desc = ':help g#' })
   keymap.set('', 'g*', '<Plug>(asterisk-gz*)', { desc = 'which_key_ignore' })
   keymap.set('', 'g#', '<Plug>(asterisk-gz#)', { desc = 'which_key_ignore' })
end

M.knap = function()
   local knap = require('knap')

   -- F5 processes the document once, and refreshes the view
   -- F6 closes the viewer application, and allows settings to be reset
   -- F7 toggles the auto-processing on and off
   -- F8 invokes a SyncTeX forward search, or similar, where appropriate
   Hydra({
      name = 'Knap',
      config = {
         color = 'teal',
         invoke_on_body = true,
         hint = false
      },
      body = '<F7>',
      heads = {
         { 'a', knap.toggle_autopreviewing, { desc = 'toggle auto-processing' } },
         { 'q', knap.close_viewer, { desc = 'close viewer' } },
         { 'o', knap.process_once, { desc = 'processes once' } },
         { 's', knap.forward_jump, { desc = 'SyncTeX forward search' } },
         { '<Esc>' }
      }
   })
end

M.neo_tree = function()
   -- keymap.set('n', [[\]], cmd 'Neotree reveal', { desc = 'File-explorer' })
   keymap.set('n', '<F3>', cmd 'Neotree toggle reveal', { desc = 'File-tree' })
end

M.nnn = function()
   keymap.set('n', '<F4>', cmd 'NnnPicker', { desc = 'nnn' })
   -- keymap.set(n, '<F3>', cmd 'NnnExplorer', { desc = 'Open file-explorer' })
end

M.draw_diagrams = function()

   local hint = [[
 Arrow^^^^^^   Select region with <C-v> 
 ^ ^ _K_ ^ ^   _f_: surround it with box
 _H_ ^ ^ _L_
 ^ ^ _J_ ^ ^                      _<Esc>_
]]


   Hydra({
      name = 'Draw Diagram',
      hint = hint,
      config = {
         color = 'pink',
         invoke_on_body = true,
         hint = {
            position = 'bottom',
            border = 'rounded'
         },
         on_enter = function()
            vim.o.virtualedit = 'all'
         end,
      },
      mode = 'n',
      body = '<leader>d',
      heads = {
         { 'H', '<C-v>h:VBox<CR>' },
         { 'J', '<C-v>j:VBox<CR>' },
         { 'K', '<C-v>k:VBox<CR>' },
         { 'L', '<C-v>l:VBox<CR>' },

         { 'f', ':VBox<CR>', { mode = 'v' }},
         { '<Esc>', nil, { exit = true } },
      }
   })
end

return M

-- vim: fml=1
