--  ██
-- ░██
-- ░██   ██  █████  ██   ██ ██████████   █████   ██████   ██████
-- ░██  ██  ██░░░██░██  ░██░░██░░██░░██ ░░░░░██ ░██░░░██ ██░░░░
-- ░█████  ░███████░██  ░██ ░██ ░██ ░██  ██████ ░██  ░██░░█████
-- ░██░░██ ░██░░░░ ░░██████ ░██ ░██ ░██ ██░░░██ ░██████  ░░░░░██
-- ░██ ░░██░░█████  ░░░░░██ ███ ░██ ░██░░███████░██░░░   ██████
-- ░░   ░░  ░░░░░    █████ ░░░  ░░  ░░  ░░░░░░░ ░██     ░░░░░░
--                  ░░░░░                       ░░

local Hydra = prequire('hydra')
local util = require('util')
local keymap = util.keymap
keymap.amend = prequire('keymap-amend')
local which_key = util.which_key
local ts_utils = prequire('nvim-treesitter.ts_utils')
local telescope_pickers = require('anuvyklack/telescope/pickers')
local cmd = require('hydra.keymap-util').cmd
local pcmd = require('hydra.keymap-util').pcmd
local api = vim.api
local M = {}

-- Move to the beginning / end of a line with "Shift + h/l"
keymap.set({ 'n','x','o' }, 'H', '^', { remap = true })
keymap.set({ 'n','x','o' }, 'L', '$', { remap = true })

keymap.set('x', '$', function() -- {{{
   -- xnoremap <expr> $ mode() == 'v' ? '$h' : '$'
   local mode = api.nvim_get_mode().mode
   if mode == 'v' then return '$h' else return '$' end
end, { expr = true }) -- }}}

keymap.set('n', 'gJ', function() require('trevj').format_at_cursor() end)

-- keymap.set('n', 'k', function() return vim.v.count > 0 and 'k' or 'gk' end,
--                      { expr = true, desc = 'k or gk' })
-- keymap.set('n', 'j', function() return vim.v.count > 0 and 'j' or 'gj' end,
--                      { expr = true, desc = 'j or gj' })

keymap.amend('n', '<Esc>', function(original) -- {{{
   local key = api.nvim_replace_termcodes('<Plug>(clever-f-reset)',
      true, true, true) --[[@as string]]
   api.nvim_feedkeys(key, 'x', false)

   if vim.v.hlsearch and vim.v.hlsearch == 1 then
      vim.cmd 'nohlsearch'
      -- vim.cmd.nohlsearch
   end

   vim.lsp.buf.clear_references()

   original()
end, { desc = 'disable search highlight' }) -- }}}

keymap.set('n', 'Q', function() -- {{{
   -- Close command line window
   -- if vim.fn.bufexists('[Command Line]') ~= 0 then
   if vim.fn.getcmdwintype() ~= '' then
      vim.cmd 'close'
      return
   end

   -- Close preview window
   for _, winnr in ipairs(api.nvim_tabpage_list_wins(0)) do
      if vim.wo[winnr].previewwindow then
         vim.cmd 'pclose'
         return
      end
   end

   -- Close quickfix window
   for _, winnr in ipairs(api.nvim_tabpage_list_wins(0)) do
      -- for _, ft in ipairs({'qf', 'quickfix'}) do
      for _, ft in ipairs({ 'qf', 'quickfix', 'help' }) do
         local bufnr = api.nvim_win_get_buf(winnr)
         if vim.bo[bufnr].buftype == ft then
            api.nvim_win_close(winnr, false)
            return
         end
      end
   end

   -- for _, winnr in ipairs(api.nvim_tabpage_list_wins(0)) do
   --    local bufnr = api.nvim_win_get_buf(winnr)
   --    if vim.bo[bufnr].filetype == 'help' then
   --       vim.cmd 'helpclose'
   --       return
   --    end
   -- end

   if #api.nvim_tabpage_list_wins(0) == 2 then
      local wins = api.nvim_tabpage_list_wins(0) ---@type integer[]
      local cur_win = api.nvim_get_current_win()
      wins = vim.tbl_filter(function(w)
         if w ~= cur_win then
            return true
         end
      end, wins) --[[@as integer[] ]]
      local win = wins[1]
      api.nvim_win_close(win, false)
   end
end, { desc = 'close service window' }) -- }}}

-- Buffers and windows managment {{{

-- Keys with '<', '>': move to previous/next
keymap.set('n', '<A-,>', cmd 'BufferPrevious')
keymap.set('n', '<A-.>', cmd 'BufferNext')
keymap.set('n', '<A-c>', cmd 'BufferClose')

keymap.set('n', '<A-<>', cmd 'BufferMovePrevious')
keymap.set('n', '<A->>', cmd 'BufferMoveNext')

local buffer_hydra = Hydra({ -- {{{
   -- name = 'Buffer',
   name = 'Barbar',
   config = {
      -- debug = true,
      on_key = function()
         -- Execute async functions synchronously to preserve animation.
         vim.wait(200, function() vim.cmd 'redraw' end, 30, false)
      end,
      -- color = 'amaranth',
      -- hint = false,
      hint = {
         -- type = 'cmdline',
         show_name = false
      },
      -- timeout = 2000,
   },
   heads = {
      { 'h', function() vim.cmd 'BufferPrevious' end, { on_key = false } },
      { 'l', function() vim.cmd 'BufferNext' end, { desc = 'choose', on_key = false } },

      { 'H', function() vim.cmd 'BufferMovePrevious' end },
      { 'L', function() vim.cmd 'BufferMoveNext' end, { desc = 'move' } },

      { 'p', function() vim.cmd 'BufferPin' end, { desc = 'pin' } },

      { 'c', function() vim.cmd 'BufferClose' end, { desc = false } },
      { 'q', function() vim.cmd 'BufferClose' end, { desc = false } },
      { 'd', function() vim.cmd 'BufferClose' end, { desc = 'close' } },

      -- { 's', function() vim.cmd 'BufferPick' end, { exit = true, desc = 'pick buffer' } },
      { 'b',  function() vim.cmd 'BufExplorer' end, { exit = true, desc = 'Explorer' } },

      -- { 'o', '<cmd>%bd|e#|bd#<CR>', { exit = true } },

      { 'od', function() vim.cmd 'BufferOrderByDirectory' end, { desc = 'by directory' } },
      { 'ol', function() vim.cmd 'BufferOrderByLanguage' end,  { desc = 'by language' } },
      { '<Esc>', nil, { exit = true } }
   }
}) -- }}}

local function choose_buffer()
   if #vim.fn.getbufinfo({ buflisted = true }) > 1 then
      buffer_hydra:activate()
   else
      vim.cmd('BufExplorer')
   end
end

keymap.set('n', 'gb', choose_buffer)

--    local window_hint = [[
--  ^^^^^       Move       ^^^^^  ^^^ Size  ^^^   ^^     Split
--  ^^^^^------------------^^^^^  ^^^-------^^^   ^^----------------
--  ^ ^ _k_ ^ ^  ^   _<C-k>_   ^   ^ ^ _K_ ^ ^    _s_: horizontally
--  _h_ ^ ^ _l_  _<C-h>_ _<C-l>_   _H_ ^ ^ _L_    _v_: vertically
--  ^ ^ _j_ ^ ^  ^   _<C-j>_   ^   ^ ^ _J_ ^ ^    _q_, _c_: close
--  ^^^^^^focus  ^^   window  ^^  ^^_=_ equal^^   _b_: choose buffer
-- ]]

-- local window_hint = [[
--  ^^^^^^^^^^^^     Move      ^^    Size   ^^   ^^     Split       ^^   Tabs
--  ^^^^^^^^^^^^-------------  ^^-----------^^   ^^---------------  ^^----------
--  ^ ^ _k_ ^ ^  ^ ^ _K_ ^ ^   ^   _<C-k>_   ^   _s_: horizontally  _t_: new tab 
--  _h_ ^ ^ _l_  _H_ ^ ^ _L_   _<C-h>_ _<C-l>_   _v_: vertically
--  ^ ^ _j_ ^ ^  ^ ^ _J_ ^ ^   ^   _<C-j>_   ^   _q_, _c_: close
--  focus^^^^^^  window^^^^^^  ^_=_: equalize^   _z_: maximize
--  ^ ^ ^ ^ ^ ^  ^ ^ ^ ^ ^ ^   ^^ ^          ^   _o_: remain only   _b_: buffers
-- ]]

-- local window_hint = [[
--  ^^^^^^^^^^^^     Move      ^^    Size   ^^   ^^     Split       ^^   Tabs
--  ^^^^^^^^^^^^-------------  ^^-----------^^   ^^---------------  ^^----------
--  ^ ^ _k_ ^ ^  ^ ^ _K_ ^ ^   ^   _<C-k>_   ^   _s_: hor, _v_: vert  _t_: new tab 
--  _h_ ^ ^ _l_  _H_ ^ ^ _L_   _<C-h>_ _<C-l>_   _q_, _c_: close
--  ^ ^ _j_ ^ ^  ^ ^ _J_ ^ ^   ^   _<C-j>_   ^   _z_: maximize
--  focus^^^^^^  window^^^^^^  ^_=_: equalize^   _o_: remain only   _b_: buffers
-- ]]

local window_hint = [[
 ^^^^^^^^^^^^     Move      ^^    Size   ^^   ^^     Split
 ^^^^^^^^^^^^-------------  ^^-----------^^   ^^---------------
 ^ ^ _k_ ^ ^  ^ ^ _K_ ^ ^   ^   _<C-k>_   ^   _s_: hor, _v_: vert 
 _h_ ^ ^ _l_  _H_ ^ ^ _L_   _<C-h>_ _<C-l>_   _q_, _c_: close
 ^ ^ _j_ ^ ^  ^ ^ _J_ ^ ^   ^   _<C-j>_   ^   _z_: maximize
 focus^^^^^^  window^^^^^^  ^_=_: equalize^   _o_: remain only
 ^^^^^ _t_:  new tab^^^^^^  ^^ ^              _b_: buffers
]]

local splits = prequire('smart-splits')

Hydra({ -- {{{
   name = 'Windows',
   hint = window_hint,
   config = { -- {{{
      -- debug = true,
      invoke_on_body = true,
      -- timeout = 4000,
      -- on_key = function() vim.wait(30) end,
      -- on_key = function()
      --    vim.wait(300, function() vim.cmd('redraw') end, 10, false)
      --    -- vim.wait(11, function()
      --    --    vim.wait(300, function() vim.cmd('redraw') end, 10, false)
      --    --    return true
      --    -- end, 10, false)
      -- end,
      hint = {
         -- type = 'window',
         border = 'rounded',
         -- offset = -1
      }
   }, -- }}}
   mode = 'n',
   body = '<C-w>',
   heads = { -- {{{
      { 'h', '<C-w>h' },
      { 'j', '<C-w>j' },
      { 'k', pcmd('wincmd k', 'E11', 'close') },
      { 'l', '<C-w>l' },

      { 'H', cmd 'WinShift left' },
      { 'J', cmd 'WinShift down' },
      { 'K', cmd 'WinShift up' },
      { 'L', cmd 'WinShift right' },

      { '<C-h>', function() splits.resize_left(2)  end },
      { '<C-j>', function() splits.resize_down(2)  end },
      { '<C-k>', function() splits.resize_up(2)    end },
      { '<C-l>', function() splits.resize_right(2) end },

      { '=', '<C-w>=', { desc = 'equalize'} },

      { 's',     pcmd('split',  'E36') },
      { '<C-s>', pcmd('split',  'E36'), { desc = false } },
      { 'v',     pcmd('vsplit', 'E36') },
      { '<C-v>', pcmd('vsplit', 'E36'), { desc = false } },

      { 'w',     '<C-w>w', { exit = true, desc = false } },
      { '<C-w>', '<C-w>w', { exit = true, desc = false } },

      { 'z',     cmd 'WindowsMaximize', { exit = true, desc = 'maximize' } },
      { '<C-z>', cmd 'WindowsMaximize', { exit = true, desc = false } },

      -- { '+', cmd 'WindowsMaximizeVertically', { exit = true } },
      -- { '|', cmd 'WindowsMaximizeHorizontally', { exit = true } },
      -- { '=', cmd 'WindowsEqualize', { exit = true } },

      { 'o',     '<C-w>o', { exit = true, desc = 'remain only' } },
      { '<C-o>', '<C-w>o', { exit = true, desc = false } },

      -- { 'p', require('nvim-window').pick, { desc = 'pick window' }},

      { 'b', choose_buffer, { exit = true, desc = 'choose buffer' } },

      { 'c',     pcmd('close', 'E444') },
      { 'q',     pcmd('close', 'E444'), { desc = 'close window' } },
      { '<C-c>', pcmd('close', 'E444'), { desc = false } },
      { '<C-q>', pcmd('close', 'E444'), { desc = false } },

      { 't', cmd 'tab split', { desc = 'new tab'} },

      { '<Esc>', nil,  { exit = true, desc = false }}
   } -- }}}
}) -- }}}

-- }}}

Hydra({ -- Quick words {{{
   name = 'Quick words',
   config = {
      -- debug = true,
      color = 'pink',
      -- hint = false,
      hint = {
         show_name = false
      },
      timeout = 6000,
   },
   mode = { 'n', 'x', 'o' },
   body = ',',
   heads = {
      { 'w', '<Plug>(smartword-w)' },
      { 'b', '<Plug>(smartword-b)' },
      { 'e', '<Plug>(smartword-e)' },
      { 'ge', '<Plug>(smartword-ge)' },
      { '<Esc>', nil, { exit = true, mode = 'n' } }
   }
}) -- }}}

-- Options hydra {{{
local options_hint = [[
  ^ ^        Options
  ^
  _v_ %{ve} virtual edit
  _i_ %{list} invisible characters  
  _s_ %{spell} spell
  _w_ %{wrap} wrap
  _c_ %{cul} cursor line
  _n_ %{nu} number
  _r_ %{rnu} relative number
  ^
       ^^^^                _<Esc>_
]]

Hydra({ -- {{{
   name = 'Options',
   hint = options_hint,
   config = {
      -- debug = true,
      color = 'amaranth',
      -- color = 'pink',
      invoke_on_body = true,
      hint = {
         -- functions = func,
         border = 'rounded',
         position = 'middle'
      }
   },
   mode = { 'n', 'x' },
   body = '<leader>o',
   heads = {
      -- { 'n', cmd 'set number!', { desc = 'number' } },
      { 'n', function() -- {{{
         if vim.o.number == true then
            vim.o.number = false
         else
            vim.o.number = true
         end
      end, { desc = 'number' } }, -- }}}
      { 'r', function() -- {{{
         if vim.o.relativenumber == true then
            vim.o.relativenumber = false
         else
            vim.o.number = true
            vim.o.relativenumber = true
         end
      end, { desc = 'relativenumber' } }, -- }}}
      { 'v', function() -- {{{
         if vim.o.virtualedit == 'all' then
            vim.o.virtualedit = 'block'
         else
            vim.o.virtualedit = 'all'
         end
      end, { desc = 'virtualedit' } }, -- }}}
      { 'i', function() -- {{{
         if vim.o.list == true then
            vim.o.list = false
         else
            vim.o.list = true
         end
      end, { desc = 'show invisible' } }, -- }}}
      { 's', function() -- {{{
         if vim.o.spell == true then
            vim.o.spell = false
         else
            vim.o.spell = true
         end
      end, { exit = true, desc = 'spell' } }, -- }}}
      { 'w', function() -- {{{
         if vim.o.wrap ~= true then
            vim.o.wrap = true
            -- Dealing with word wrap:
            -- If cursor is inside very long line in the file than wraps
            -- around several rows on the screen, then 'j' key moves you to
            -- the next line in the file, but not to the next row on the
            -- screen under your previous position as in other editors. These
            -- bindings fixes this.
            vim.keymap.set('n', 'k', function() return vim.v.count > 0 and 'k' or 'gk' end,
               { expr = true, desc = 'k or gk' })
            vim.keymap.set('n', 'j', function() return vim.v.count > 0 and 'j' or 'gj' end,
               { expr = true, desc = 'j or gj' })
         else
            vim.o.wrap = false
            vim.keymap.del('n', 'k')
            vim.keymap.del('n', 'j')
         end
      end, { desc = 'wrap' } }, -- }}}
      { 'c', function() -- {{{
         if vim.o.cursorline == true then
            vim.o.cursorline = false
         else
            vim.o.cursorline = true
         end
      end, { desc = 'cursor line' } },
      { '<Esc>', nil, { exit = true } } -- }}}
   }
}) -- }}}
-- }}}

Hydra({ -- Folds {{{
   name = 'Folds',
   config = {
      hint = {
         show_name = false,
      },
   },
   body = 'z',
   heads = {
      { 'j', 'zj' },
      { 'k', 'zk', { desc = '↓ ↑'} },
      -- { 'h', 'h', { remap = true, desc = 'preview' } },
      { '<Esc>', nil, { exit = true, desc = false } },
   }
}) -- }}}

keymap.set('n', '<C-g>', cmd 'TSHighlightCapturesUnderCursor')
-- nnoremap <C-g> <cmd>call SyntaxAttr()<CR>

Hydra({ -- Side-scroll {{{
   name = 'Side scroll',
   config = {
      -- debug = true,
      -- timeout = 2000,
      hint = false
      -- hint = 'statusline'
   },
   mode = 'n',
   body = 'z',
   heads = {
      { 'h', '5zh' },
      { 'l', '5zl', { desc = '←/→' } },
      { 'H', 'zH' },
      { 'L', 'zL', { desc = 'half screen ←/→' } },
   }
}) -- }}}

--------------------------------------------------------------------------------

M.yanky = function() -- {{{
   local yanky = require('yanky')

   keymap.set({ 'n', 'x' }, 'p', '<Plug>(YankyPutAfter)')
   keymap.set({ 'n', 'x' }, 'P', '<Plug>(YankyPutBefore)')

   -- keymap.set('n', '<C-n>', '<Plug>(YankyCycleForward)')
   -- keymap.set('n', '<C-p>', '<Plug>(YankyCycleBackward)')

   keymap.amend('n', '<C-p>', function(original)
      if yanky.can_cycle() then
         yanky.cycle(-1)
      else
         original()
      end
   end)

   keymap.amend('n', '<C-n>', function(original)
      if yanky.can_cycle() then
         yanky.cycle(1)
      else
         original()
      end
   end)

end -- }}}

M.lsp = function(bufnr) -- {{{
   local opts = setmetatable({ buffer = bufnr }, { -- {{{
      __call = function(self, input)
         local opts = vim.deepcopy(self)
         for key, value in pairs(input) do
            opts[key] = value
         end
         return opts
      end
   }) -- }}}

   keymap.set('n', 'gd', vim.lsp.buf.definition, opts { desc = 'LSP go to definition' })
   keymap.set('n', 'gD', vim.lsp.buf.declaration, opts { desc = 'LSP go to declaration' })
   keymap.set('n', 'gi', vim.lsp.buf.implementation, opts { desc = 'LSP list implementations' })
   -- keymap.set('n', 'gd', require('goto-preview').goto_preview_definition)
   -- keymap.set('n', 'gi', require('goto-preview').goto_preview_implementation)
   keymap.set('n', 'gr', vim.lsp.buf.references, opts { desc = 'LSP references' })
   -- keymap.set('n', 'gR', cmd 'TroubleToggle lsp_references', opts { desc = 'LSP references', requires = 'trouble' })

   keymap.set('n', 'K', vim.lsp.buf.hover, opts { desc = 'hover doc', ft_ignore = { 'vim' } })

   -- keymap.set{'n', '#', function() -- {{{
   --    local node = ts_utils.get_node_at_cursor()
   --    while node ~= nil do
   --       local node_type = node:type()
   --       if node_type == "string"
   --           or node_type == "string_fragment"
   --           or node_type == "template_string"
   --           or node_type == "document" -- for inline gql`` strings
   --       then
   --          -- who wants to highlight a string?
   --          return
   --       end
   --       node = node:parent()
   --    end
   --    vim.lsp.buf.document_highlight()
   -- end, { desc = 'highlight references' } } -- }}}

   keymap.set('n', '#', vim.lsp.buf.document_highlight, opts{ desc = 'LSP highlight references' })

   which_key.name('n', '<leader>l', 'LSP')

--    local hint = [[
--  ^^                    ^^       Telescope
--  ^^-----------------   ^^----------------------- 
--  _r_ rename            _te_ buffer diagnostic
--  _a_ code action       _tE_ workspace diagnostic 
--  _s_ signature help    _td_ definitions
--  _t_ type definition   _tr_ buffer references
--  _f_ format            _tR_ references
--  _v_ Vista             _ti_ implementation
--                        _ts_ document symbols
--                        _tS_ workspace symbols
-- ]]

--    local hint = [[
--  ^^                    ^^      Telescope
--  ^^-----------------   ^^-------------------- 
--  _r_ rename            _td_ definitions
--  _a_ code action       _tr_ buffer references
--  _s_ signature help    _tR_ references
--  _t_ type definition   _ti_ implementation
--  _f_ format            _ts_ document symbols
--  _v_ Vista             _tS_ workspace symbols
-- ]]

   local hint = [[
 ^^                    ^^      Telescope
 ^^-----------------   ^^-------------------- 
 _r_ rename            _td_ definitions
 _a_ code action       _tr_ buffer references
 _s_ signature help    _tR_ references
 _t_ type definition   _ti_ implementation
 _f_ format            _ts_ document symbols
 _v_ Vista             _tS_ workspace symbols
]]

   Hydra { -- {{{
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
      mode = { 'n', 'x' },
      body = '<leader>l',
      heads = {
         { 'r', vim.lsp.buf.rename, { desc = 'rename' } },
         -- { 'r', prequire('renamer').rename, { desc = 'rename' } },

         { 'a', vim.lsp.buf.code_action, { desc = 'code action' } },

         { 's', vim.lsp.buf.signature_help, { desc = 'sinature help' } },
         { 't', vim.lsp.buf.type_definition, { desc = 'type definition' } },
         { 'f', vim.lsp.buf.formatting, { desc = 'format' } },

         { 'td', telescope_pickers.definitions, { desc = 'definitions' } },
         { 'tr', telescope_pickers.buffer_references, { desc = 'buffer references' } },
         { 'tR', telescope_pickers.references, { desc = 'references' } },

         { 'ti', telescope_pickers.implementations, { desc = 'implementations' } },

         { 'ts', telescope_pickers.document_symbols, { desc = 'symbols' } },
         { 'tS', telescope_pickers.workspace_symbols, { desc = 'workspace symbols' } },

         { 'v', cmd 'Vista nvim_lsp', { desc = 'Vista' } },
         { '<Esc>', nil, { exit = true } }
      }
   } -- }}}

   -- -- Workspace
   -- which_key.name('n', '<leader>lw', 'workspace')
   -- keymap.set('n', '<leader>lwl',
   --    function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end,
   --    opts{ desc = 'list workspace folders' })
   -- keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder,    opts{ desc = 'add workspace folder' })
   -- keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, opts{ desc = 'remove workspace folder' })

end -- }}}

-- Diagnostic {{{
-- local diagnostic_hint = [[
-- -- ]]

keymap.set('n', '[d', vim.diagnostic.goto_prev)
keymap.set('n', ']d', vim.diagnostic.goto_next)

local diagnostics_active = true

Hydra({
   name = 'diagnostic',
   -- hint = diagnostic_hint,
   config = {
      color = 'teal',
      invoke_on_body = true,
      hint = {
         -- position = 'middle-right',
         border = 'rounded'
      }
   },
   mode = 'n',
   body = '<leader>d',
   heads = {
      { 's', function() -- {{{
         vim.diagnostic.config({
            -- virtual_lines = { only_current_line = true },
            virtual_text = not vim.diagnostic.config().virtual_text,
            virtual_lines = not vim.diagnostic.config().virtual_lines,
         })
      end, { desc = 'show lines' } }, -- }}}
      { 't', function() -- {{{
         diagnostics_active = not diagnostics_active
         if diagnostics_active then
            vim.diagnostic.show()
            -- vim.diagnostic.disable()
         else
            vim.diagnostic.hide()
            -- vim.diagnostic.enable()
         end
      end, { desc = 'toggle' } }, -- }}}
      { 'q', vim.diagnostic.setloclist, { desc = 'loclist' } },
      { '<Esc>', nil }
   }
})
-- }}}

M.telescope = function() -- {{{

   -- hint {{{

   --    local hint = [[
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
   --    local hint = [[

   --    🭇🬭🬭🬭🬭🬭🬭🬭🬭🬼
   --   🭉🭁🭠🭘    🭣🭕🭌🬾  _f_: files       _m_: marks            _h_: vim help   _c_: execute command
   --   🭅█ ▁     █🭐  _o_: old files   _g_: live grep        _k_: keymap     _;_: commands history
   --   ██🬿      🭊██  _p_: projects    _/_: search in file   _r_: registers  _?_: search history
   --  🭋█🬝🮄🮄🮄🮄🮄🮄🮄🮄🬆█🭀 ^
   --  🭤🭒🬺🬹🬱🬭🬭🬭🬭🬵🬹🬹🭝🭙 ^ ^              ^ ^        _<Enter>_: Telescope       ^ ^            _<Esc>_
   -- ]]

   local hint = [[
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

   -- }}}

   Hydra({ -- {{{
      name = 'Telescope',
      hint = hint,
      config = {
         color = 'teal',
         invoke_on_body = true,
         hint = {
            -- type = 'cmdline',
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
         { '?', cmd 'Telescope search_history', { desc = 'search history' } },

         { ';', cmd 'Telescope command_history', { desc = 'command-line history' } },
         { 'c', cmd 'Telescope commands', { desc = 'execute command' } },

         { 'u', cmd 'silent! %foldopen! | UndotreeToggle', { desc = 'undotree' } },

         -- { 'j', ':lua require"utils.telescope".jump()<CR>' },
         -- { 'l', telescope.extensions.neoclip.default },
         -- { 'z', telescope.extensions.zoxide.list },

         { '<Enter>', cmd 'Telescope', { exit = true, desc = 'list all pickers' } },
         { '<Esc>', nil, { exit = true, nowait = true } },
      }
   }) -- }}}

   keymap.set('n', 'z=', cmd 'Telescope spell_suggest', { desc = 'spell Suggest' })
end -- }}}

M.gitsigns = function(bufnr) -- {{{
   local gitsigns = prequire('gitsigns')

   local hint = [[
 _J_: next hunk   _s_: stage hunk        _d_: show deleted   _b_: blame line
 _K_: prev hunk   _u_: undo last stage   _p_: preview hunk   _B_: blame show full 
 ^ ^              _S_: stage buffer      ^ ^                 _/_: show base file 
 ^
 ^ ^              _<Enter>_: Neogit              _q_: exit
]]

   -- Hydra({ -- {{{
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
   --             local mode = api.nvim_get_mode().mode:sub(1,1)
   --             if mode == 'V' then -- visual-line mode
   --                local esc = api.nvim_replace_termcodes('<Esc>', true, true, true)
   --                api.nvim_feedkeys(esc, 'x', false) -- exit visual mode
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
   -- }) -- }}}

   Hydra({ -- {{{
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
            vim.cmd 'mkview'
            vim.cmd 'silent! %foldopen!'
            vim.bo.modifiable = false
            gitsigns.toggle_signs(true)
            gitsigns.toggle_linehl(true)
            vim.wait(50, function() vim.cmd 'redraw' end, 10, false)
         end,
         on_exit = function()
            local cursor_pos = api.nvim_win_get_cursor(0)
            vim.cmd 'loadview'
            api.nvim_win_set_cursor(0, cursor_pos)
            -- api.nvim_feedkeys('zv', '', false)
            vim.cmd 'normal zv'
            gitsigns.toggle_signs(false)
            gitsigns.toggle_linehl(false)
            gitsigns.toggle_deleted(false)
         end,
      },
      mode = { 'n', 'x' },
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
         { 'B', function() gitsigns.blame_line { full = true } end, { desc = 'blame show full' } },

         { '/', gitsigns.show, { exit = true, desc = 'show base file' } }, -- show the base of the file

         { '<Enter>', cmd 'Neogit', { exit = true, desc = 'Neogit' } },

         { 'q', nil, { exit = true, nowait = true, desc = 'exit' } },
         -- { '<Esc>', nil, { exit = true, desc = 'exit' } }
      }
   }) -- }}}

end -- }}}

M.hop = function() -- {{{
   keymap.set({ 'n', 'x' }, ';w', cmd 'HopWordAC', { desc = 'Easymotion forward word' })
   keymap.set({ 'n', 'x' }, ';b', cmd 'HopWordBC', { desc = 'Easymotion bacward word' })

   keymap.set({ 'n', 'x' }, ';j', cmd 'HopLineAC', { desc = 'Easymotion line up' })
   keymap.set({ 'n', 'x' }, ';k', cmd 'HopLineBC', { desc = 'Easymotion line down' })

   keymap.set({ 'n', 'x' }, 's', cmd 'HopChar1', { desc = 'Easymotion char' })

   keymap.set('n', 't', cmd 'HopChar2', { desc = 'Easymotion 2 chars' })
end -- }}}

M.easy_align = function() -- {{{
   -- n : interactive EasyAlign for a motion/text object (e.g. gaip)
   -- x : interactive EasyAlign in visual mode (e.g. vipga)
   keymap.set({ 'n', 'x' }, '<leader>a', '<Plug>(EasyAlign)')
end -- }}}

M.iswap = function() -- {{{
   keymap.set({ 'n', 'x' }, 'gs', cmd 'ISwap')
end -- }}}

M.luasnip = function() -- {{{
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

   keymap.amend({ 'i', 's' }, '<C-n>', function(original)
      if luasnip.choice_active() then
         luasnip.change_choice(1)
      else
         original()
      end
   end)

   keymap.amend({ 'i', 's' }, '<C-p>', function(original)
      if luasnip.choice_active() then
         luasnip.change_choice(-1)
      else
         original()
      end
   end)

end -- }}}

M.asterisks = function() -- {{{
   keymap.set('', '*', '<Plug>(asterisk-z*)')
   -- keymap.set('', '#', '<Plug>(asterisk-z#)')
   -- keymap.set('', 'g*', '<Plug>(asterisk-gz*)', { desc = ':help gstar' })
   -- keymap.set('', 'g#', '<Plug>(asterisk-gz#)', { desc = ':help g#' })
   keymap.set('', 'g*', '<Plug>(asterisk-gz*)', { desc = 'which_key_ignore' })
   -- keymap.set('', 'g#', '<Plug>(asterisk-gz#)', { desc = 'which_key_ignore' })
end -- }}}

M.knap = function() -- {{{
   local knap = require('knap')

   -- F5 processes the document once, and refreshes the view
   -- F6 closes the viewer application, and allows settings to be reset
   -- F7 toggles the auto-processing on and off
   -- F8 invokes a SyncTeX forward search, or similar, where appropriate
   Hydra({
      name = 'Knap',
      config = {
         -- debug = true,
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
end -- }}}

M.file_tree = function() -- {{{
   if pcall(require, 'nvim-tree') then
      keymap.set('n', '<F3>', cmd 'NvimTreeToggle')
   elseif pcall(require, 'neo-tree') then
      keymap.set('n', '<F3>', cmd 'Neotree toggle reveal')
      -- keymap.set('n', [[\]], cmd 'Neotree reveal', { desc = 'File-explorer' })
   end
end -- }}}

M.nnn = function() -- {{{
   keymap.set('n', '<F4>', cmd 'NnnPicker', { desc = 'nnn' })
   -- keymap.set(n, '<F3>', cmd 'NnnExplorer', { desc = 'Open file-explorer' })
end -- }}}

M.draw_diagrams = function() -- {{{

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
      body = '<leader>e',
      heads = {
         { 'H', '<C-v>h:VBox<CR>' },
         { 'J', '<C-v>j:VBox<CR>' },
         { 'K', '<C-v>k:VBox<CR>' },
         { 'L', '<C-v>l:VBox<CR>' },

         { 'f', ':VBox<CR>', { mode = 'v' } },
         { '<Esc>', nil, { exit = true } },
      }
   })
end -- }}}

return M

-- vim: fdm=marker fml=1
