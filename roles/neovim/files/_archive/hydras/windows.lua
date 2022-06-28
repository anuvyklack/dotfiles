local Hydra = require('hydra')
local splits = require('smart-splits')

-- local hint = [[
-- Move focus ^^^^^^^ Move window
--  ^ ^   _k_   ^ ^   ^ ^   _K_   ^ ^   ^     ^ _<C-k>_ ^     ^
--  ^ ^   ^↑^   ^ ^   ^ ^   ^↑^   ^ ^   ^     ^  ^ ↑ ^  ^     ^
--  _h_ ← ^ ^ → _l_   _H_ ← ^ ^ → _L_   _<C-h>_ ←^   ^→ _<C-l>_
--  ^ ^   ^↓^   ^ ^   ^ ^   ^↓^   ^ ^   ^     ^  ^ ↓ ^  ^     ^
--  ^ ^   _j_   ^ ^   ^ ^   _J_   ^ ^   ^     ^ _<C-j>_ ^     ^
-- ]]

local hint = [[
 ^^^^^^     Move     ^^^^^^   ^^    Size   ^^   ^^     Split
 ^^^^^^--------------^^^^^^   ^^-----------^^   ^^---------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^    ^   _<C-k>_   ^   _s_: horizontally
 _h_ ^ ^ _l_   _H_ ^ ^ _L_    _<C-h>_ _<C-l>_   _v_: vertically
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^    ^   _<C-j>_   ^   _q_: close
 focus^^^^^^   window^^^^^^   ^_=_: equalize    ^ ^
 ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^    ^^ ^          ^   _b_: choose buffer 
]]

-- local hint = [[
--  ^^^^^^     Move     ^^^^^^   ^^    Size   ^^   ^^     Split
--  ^^^^^^--------------^^^^^^   ^^-----------^^   ^^---------------
--  ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^    _<C-j>_ _<C-k>_   _s_: horizontally
--  _h_ ^ ^ _l_   _H_ ^ ^ _L_    _<C-h>_ _<C-l>_   _v_: vertically
--  ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^    ^_=_: equalize    _q_: close
--  focus^^^^^^   window^^^^^^
--  ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^    ^^ ^          ^   _b_: choose buffer 
-- ]]

local windows = {
   name = 'WINDOWS',
   hint = hint,
   config = {
      timeout = 4000,
      hint = {
         border = 'rounded',
         position = 'top'
      }
   },
   mode = 'n',
   body = '<C-w>',
   heads = {
      { 'h', '<C-w>h' },
      { 'j', '<C-w>j' },
      { 'k', [[<cmd>try | wincmd k | catch /^Vim\%((\a\+)\)\=:E11:/ | close | endtry<CR>]] },
      { 'l', '<C-w>l' },

      { 'H', '<Cmd>WinShift left<CR>' },
      { 'J', '<Cmd>WinShift down<CR>' },
      { 'K', '<Cmd>WinShift up<CR>' },
      { 'L', '<Cmd>WinShift right<CR>' },

      { 's', '<C-w>s' },
      { 'v', '<C-w>v' },

      { '<C-h>', function() splits.resize_left(2)  end },
      { '<C-j>', function() splits.resize_down(2)  end },
      { '<C-k>', function() splits.resize_up(2)    end },
      { '<C-l>', function() splits.resize_right(2) end },
      { '=', '<C-w>=', { desc = 'equalize'} },

      -- { 'w', require('nvim-window').pick, { exit = true, desc = 'choose window' }},
      { 'b', '<Cmd>call ChooseBuffer()<CR>', { exit = true, desc = 'choose buffer' } },
      -- { 'b', '<Cmd>BufExplorer<CR>', { exit = true, desc = 'choose buffer' } },

      -- { 'q', '<Cmd>try | close | catch | endtry<CR>', { desc = 'close window' } },
      { 'q', [[<Cmd>try | close | catch /^Vim\%((\a\+)\)\=:E444:/ | endtry<CR>]],
                                                     { desc = 'close window' } },
      { '<Esc>', nil,  { exit = true, desc = false }}
   }
}

Hydra(windows)
