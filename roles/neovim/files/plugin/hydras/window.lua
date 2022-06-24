local Hydra = require('hydra')

-- local hint = [[
-- Move focus ^^^^^^^ Move window
--  ^ ^   _k_   ^ ^   ^ ^   _K_   ^ ^
--  ^ ^   ^↑^   ^ ^   ^ ^   ^↑^   ^ ^
--  _h_ ← ^ ^ → _l_   _H_ ← ^ ^ → _L_
--  ^ ^   ^↓^   ^ ^   ^ ^   ^↓^   ^ ^
--  ^ ^   _j_   ^ ^   ^ ^   _J_   ^ ^
-- ]]

local hint = [[
 ^^^^^^     Move     ^^^^^^   ^^     Split         ^^^^    Size
 ^^^^^^--------------^^^^^^   ^^---------------    ^^^^------------- 
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^    _s_: horizontally    _+_ _-_: height
 _h_ ^ ^ _l_   _H_ ^ ^ _L_    _v_: vertically      _>_ _<_: width
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^    _q_: close           ^ _=_ ^: equalize
 focus^^^^^^   window^^^^^^ 
 ^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^    _b_: choose buffer   ^ ^ ^ ^    _<Esc>_
]]

local window = {
   hint = hint,
   config = {
      timeout = 4000,
      hint = {
         border = 'rounded'
      }
   },
   mode = 'n',
   body = '<C-w>',
   heads = {
      { 'h', '<C-w>h' },
      { 'j', '<C-w>j' },
      { 'k', '<C-w>k' },
      { 'l', '<C-w>l' },

      { 'H', '<Cmd>WinShift left<CR>' },
      { 'J', '<Cmd>WinShift down<CR>' },
      { 'K', '<Cmd>WinShift up<CR>' },
      { 'L', '<Cmd>WinShift right<CR>' },

      { 's', '<C-w>s' },
      { 'v', '<C-w>v' },

      { '+', '<C-w>+' },
      { '-', '<C-w>-' },

      { '>', '2<C-w>>', { desc = 'increase width' } },
      { '<', '2<C-w><', { desc = 'decrease width' } },

      -- { '>', '<Cmd>vertical resize +2<CR>', { desc = 'increase width' } },
      -- { '<', '<Cmd>vertical resize -2<CR>', { desc = 'decrease width' } },

      { '=', '<C-w>=', { desc = 'equalize'} },

      -- { 'w', require('nvim-window').pick, { exit = true, desc = 'choose window' }},
      { 'b', '<Cmd>call ChooseBuffer()<CR>', { exit = true, desc = 'choose buffer' } },

      -- { 'q', [[<Cmd>try | close | catch /^Vim\%((\a\+)\)\=:E444:/ | endtry<CR>]] },
      { 'q', '<Cmd>try | close | catch | endtry<CR>', { desc = 'close window' } },

      { '<Esc>', nil,  { exit = true }}
   }
}

Hydra(window)
