--  ██                           ██      ██              ██ ██
-- ░██                          ░██     ░░              ░██░░
-- ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
-- ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
-- ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
-- ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
-- ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
-- ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
--                  ░░░░░                                              ░░░░░

local util = require('util')
local prequire = util.prequire
local Hydra = prequire("hydra")
local keymap = util.keymap
local cmd = keymap.cmd
keymap.amend = prequire('keymap-amend')

-- -- Dealing with word wrap:
-- -- If cursor is inside very long line in the file than wraps around several rows
-- -- on the screen, then 'j' key moves you to the next line in the file, but not
-- -- to the next row on the screen under your previous position as in other
-- -- editors. These bindings fixes this.
-- keymap.set('n', 'k', function() return vim.v.count > 0 and 'k' or 'gk' end, { silent = true, expr = true })
-- keymap.set('n', 'j', function() return vim.v.count > 0 and 'j' or 'gj' end, { silent = true, expr = true })

do -- Buffers and windows managment hydras

   -- Keys with '<', '>': move to previous/next
   keymap.set('n', '<A-,>', cmd 'BufferPrevious')
   keymap.set('n', '<A-.>', cmd 'BufferNext')

   local buffer_hydra = Hydra({ -- {{{
      -- name = 'Buffer',
      name = 'Barbar',
      config = {
         -- debug = true,
         on_key = function()
            -- Execute async functions synchronously to preserve animation.
            vim.wait(200, function() vim.cmd 'redraw' end, 30, false)
         end
         -- color = 'amaranth',
         -- hint = false,
         -- hint = {
         --    position = 'top'
         -- },
         -- timeout = 2000,
      },
      heads = {
         { 'h', function() vim.cmd 'BufferPrevious' end, { on_key = false } },
         { 'l', function() vim.cmd 'BufferNext' end, { desc = 'choose', on_key = false } },

         { 'H', function() vim.cmd 'BufferMovePrevious' end },
         { 'L', function() vim.cmd 'BufferMoveNext' end, { desc = 'move' } },

         { 'p', function() vim.cmd 'BufferPin' end, { desc = 'pin' } },

         { 'd', function() vim.cmd 'BufferClose' end, { desc = 'close' } },
         { 'c', function() vim.cmd 'BufferClose' end, { desc = false } },
         { 'q', function() vim.cmd 'BufferClose' end, { desc = false } },

         -- { 's', function() vim.cmd 'BufferPick' end, { exit = true, desc = 'pick buffer' } },
         { 'b',  function() vim.cmd 'BufExplorer' end, { exit = true, desc = 'Explorer' } },
         { 'od', function() vim.cmd 'BufferOrderByDirectory' end, { desc = 'by directory' } },
         { 'ol', function() vim.cmd 'BufferOrderByLanguage' end,  { desc = 'by language' } },
         { '<Esc>', nil, { exit = true } }
      }
   }) -- }}}

   local function choose_buffer()
      if #vim.fn.getbufinfo({ buflisted = true }) > 1 then
         buffer_hydra:activate()
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

   local window_hint = [[
 ^^^^^^^^^^^^     Move      ^^    Size   ^^   ^^     Split
 ^^^^^^^^^^^^-------------  ^^-----------^^   ^^---------------
 ^ ^ _k_ ^ ^  ^ ^ _K_ ^ ^   ^   _<C-k>_   ^   _s_: horizontally 
 _h_ ^ ^ _l_  _H_ ^ ^ _L_   _<C-h>_ _<C-l>_   _v_: vertically
 ^ ^ _j_ ^ ^  ^ ^ _J_ ^ ^   ^   _<C-j>_   ^   _q_, _c_: close
 focus^^^^^^  window^^^^^^  ^_=_: equalize^   _z_: maximize
 ^ ^ ^ ^ ^ ^  ^ ^ ^ ^ ^ ^   ^^ ^          ^   _o_: remain only
 _b_: choose buffer
]]

   local splits = prequire('smart-splits')

   -- keymap.set('n', '<C-w>z', cmd 'MaximizerToggle!', { remap = true })

   Hydra({
      name = 'Windows',
      hint = window_hint,
      config = {
         -- debug = true,
         invoke_on_body = true,
         -- timeout = 4000,
         hint = {
            border = 'rounded',
            -- position = 'middle'
         }
      },
      mode = 'n',
      body = '<C-w>',
      heads = {
         { 'h', '<C-w>h' },
         { 'j', '<C-w>j' },
         { 'k', cmd [[try | wincmd k | catch /^Vim\%((\a\+)\)\=:E11:/ | close | endtry]] },
         { 'l', '<C-w>l' },

         -- { '<C-h>', cmd 'WinShift left' },
         -- { '<C-j>', cmd 'WinShift down' },
         -- { '<C-k>', cmd 'WinShift up' },
         -- { '<C-l>', cmd 'WinShift right' },
         --
         -- { 'H', function() splits.resize_left(2)  end },
         -- { 'J', function() splits.resize_down(2)  end },
         -- { 'K', function() splits.resize_up(2)    end },
         -- { 'L', function() splits.resize_right(2) end },

         { 'H', cmd 'WinShift left' },
         { 'J', cmd 'WinShift down' },
         { 'K', cmd 'WinShift up' },
         { 'L', cmd 'WinShift right' },

         { '<C-h>', function() splits.resize_left(2)  end },
         { '<C-j>', function() splits.resize_down(2)  end },
         { '<C-k>', function() splits.resize_up(2)    end },
         { '<C-l>', function() splits.resize_right(2) end },
         { '=', '<C-w>=', { desc = 'equalize'} },

         { 's', '<C-w>s' }, { '<C-s>', '<C-w><C-s>', { desc = false } },
         { 'v', '<C-w>v' }, { '<C-v>', '<C-w><C-v>', { desc = false } },

         { 'w',     '<C-w>w', { exit = true, desc = false } },
         { '<C-w>', '<C-w>w', { exit = true, desc = false } },

         { 'z', cmd 'MaximizerToggle!', { desc = 'maximize' } },
         { '<C-z>', cmd 'MaximizerToggle!', { exit = true, desc = false } },

         { 'o', '<C-w>o', { exit = true, desc = 'remain only' } },
         { '<C-o>', '<C-w>o', { exit = true, desc = false } },

         -- { 'p', require('nvim-window').pick, { desc = 'pick window' }},

         { 'b', choose_buffer, { exit = true, desc = 'choose buffer' } },

         { 'c', cmd [[try | close | catch /^Vim\%((\a\+)\)\=:E444:/ | endtry]] },
         { 'q', cmd [[try | close | catch /^Vim\%((\a\+)\)\=:E444:/ | endtry]], { desc = 'close window' } },
         { '<C-q>', cmd [[try | close | catch /^Vim\%((\a\+)\)\=:E444:/ | endtry]], { desc = false } },
         { '<C-c>', cmd [[try | close | catch /^Vim\%((\a\+)\)\=:E444:/ | endtry]], { desc = false } },

         { '<Esc>', nil,  { exit = true, desc = false }}
      }
   })

end

Hydra({ -- Side-scroll
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
})

Hydra({ -- Quick words
   name = 'Quick words',
   config = {
      -- debug = true,
      color = 'pink',
      -- color = 'amaranth',
      hint = false,
      -- hint = 'statusline',
      timeout = 6000,
   },
   mode = {'n','x','o'},
   body = ',',
   heads = {
      -- { 'w',  '<Plug>(quickword-w)' },
      -- { 'b',  '<Plug>(quickword-b)' },
      -- { 'e',  '<Plug>(quickword-e)' },
      -- { 'ge', '<Plug>(quickword-ge)' },

      { 'w',  '<Plug>(smartword-w)'  },
      { 'b',  '<Plug>(smartword-b)'  },
      { 'e',  '<Plug>(smartword-e)'  },
      { 'ge', '<Plug>(smartword-ge)' },

      { '<Esc>', nil, { exit = true, mode = 'n' } }
   }
})

do -- Options hydra

--    local hint = [[
--   ^^^^^         Options 
--   ^
--   %{nu}^^^  _n_umber
--   %{rnu}^^  _r_elative number  
--   %{ve}^^^  _v_irtual edit
--   %{list}^  _i_nvisible characters  
--   %{spell}  _s_pell
--   %{wrap}^  _w_rap
--   %{cul}^^  _c_ursor line
--   ^
--   ^^^^^                    _<Esc>_
-- ]]

   local hint = [[
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

   Hydra({ -- Options
      name = 'Options',
      hint = hint,
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
      mode = {'n','x'},
      body = '<leader>o',
      heads = {
         -- { 'n', cmd 'set number!', { desc = 'number' } },
         { 'n', function()
            if vim.o.number == true then
               vim.o.number = false
            else
               vim.o.number = true
            end
         end, { desc = 'number' } },
         { 'r', function()
            if vim.o.relativenumber == true then
               vim.o.relativenumber = false
            else
               vim.o.number = true
               vim.o.relativenumber = true
            end
         end, { desc = 'relativenumber' } },
         { 'v', function()
            if vim.o.virtualedit == 'all' then
               vim.o.virtualedit = 'block'
            else
               vim.o.virtualedit = 'all'
            end
         end, { desc = 'virtualedit' } },
         { 'i', function()
            if vim.o.list == true then
               vim.o.list = false
            else
               vim.o.list = true
            end
         end, { desc = 'show invisible' } },
         { 's', function()
            if vim.o.spell == true then
               vim.o.spell = false
            else
               vim.o.spell = true
            end
         end, { exit = true, desc = 'spell' } },
         { 'w', function()
            if vim.o.wrap == true then
               vim.o.wrap = false
            else
               vim.o.wrap = true
            end
         end, { desc = 'wrap' } },
         { 'c', function()
            if vim.o.cursorline == true then
               vim.o.cursorline = false
            else
               vim.o.cursorline = true
            end
         end, { desc = 'cursor line' } },
         -- { 'b', function()
         --    if vim.o.background == 'dark' then
         --       vim.o.background = 'light'
         --    else
         --       vim.o.background = 'dark'
         --    end
         -- end, { desc = 'background' } },
         -- { 'cc', function()
         --    if vim.o.cursorcolumn == true then
         --       vim.o.cursorcolumn = false
         --    else
         --       vim.o.cursorcolumn = true
         --    end
         -- end, { desc = 'cursor column' } },
         -- { 'cx', function()
         --    if vim.o.cursorline == true and vim.o.cursorcolumn then
         --       vim.o.cursorline = false
         --       vim.o.cursorcolumn = false
         --    else
         --       vim.o.cursorline = true
         --       vim.o.cursorcolumn = true
         --    end
         -- end, { desc = 'cursor cross' } },
         { '<Esc>', nil, { exit = true } }
      }
   })
end

-- Turn off highlight of recently searched text
-- keymap.set('n', '<Esc>', '<Cmd>nohlsearch<CR><Esc>')
keymap.amend('n', '<Esc>', function(original)
   local key = vim.api.nvim_replace_termcodes('<Plug>(clever-f-reset)', true, true, true) --[[@as string]]
   vim.api.nvim_feedkeys(key, 'x', false)

   if vim.v.hlsearch and vim.v.hlsearch == 1 then
      vim.cmd 'nohlsearch'
   end
   original()
end, { desc = 'disable search highlight' })

keymap.set('n', 'Q', function()
   -- Close command line window
   if vim.fn.bufexists("[Command Line]") ~= 0 then
      vim.cmd 'close'
      return
   end

   -- Close preview window
   for _, winnr in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
      if vim.wo[winnr].previewwindow then
         vim.cmd 'pclose'
         return
      end
   end

   -- Close quickfix window
   for _, winnr in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
      -- for _, ft in ipairs({'qf', 'quickfix'}) do
      for _, ft in ipairs({'qf', 'quickfix', 'help'}) do
         local bufnr = vim.api.nvim_win_get_buf(winnr)
         if vim.bo[bufnr].buftype == ft then
            vim.api.nvim_win_close(winnr, false)
            return
         end
      end
   end

   -- for _, winnr in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
   --    local bufnr = vim.api.nvim_win_get_buf(winnr)
   --    if vim.bo[bufnr].filetype == 'help' then
   --       vim.cmd 'helpclose'
   --       return
   --    end
   -- end

   if #vim.api.nvim_tabpage_list_wins(0) == 2 then
      local wins = vim.api.nvim_tabpage_list_wins(0) ---@type integer[]
      local cur_win = vim.api.nvim_get_current_win()
      wins = vim.tbl_filter(function(w)
         if w ~= cur_win then
            return true
         end
      end, wins) --[[@as integer[] ]]
      local win = wins[1]
      vim.api.nvim_win_close(win, false)
   end
end, { desc = 'close service window' })

keymap.set('n', '<leader>j', function() require('trevj').format_at_cursor() end)

Hydra({
     name = "Move",
     mode = "n",
     body = "<Space>z",
     config = {
         debug = true,
         color = 'amaranth',
     },
     heads = {
        -- Move Up and Down
        { "k"    , "<C-u>"},
        { "j"    , "<C-d>", {desc = [[\^/v]]} },
        { "l"    , "zh"},
        { "r"    , "zl", {desc = "</>"}},

        -- Move Faster
        { "U"    , "<C-b>"},
        { "D"    , "<C-f>", {desc = [[page \^/v]]}},
        { "R"    , "zL"},
        { "L"    , "zR", {desc = "half screen </>"}},

        -- Move view around cursor
        { "t"    , "zt"},
        { "b"    , "zb", {desc = [[around line \^/v]]}},
        { "m"    , "zz", {desc = "middle"}},
        -- { "l"    , "zs"},
        -- { "r"    , "ze", {desc = "</>"}},
    },
})
