local Hydra = require('hydra')
local gitsigns = require('gitsigns')

local hint = [[
 _J_: next hunk   _s_: stage hunk        _d_: show deleted   _b_: blame line
 _K_: prev hunk   _u_: undo last stage   _p_: preview hunk   _B_: blame show full 
 ^ ^              _S_: stage buffer      ^ ^                 _/_: show base file
 ^
 ^ ^              _<Enter>_: Neogit              _q_: exit
]]

-- local hint_2 =
-- [[
--  ^ ^              ^ ^                    ^ ^                 ^ ^          ╭─────╮
--  ^ ^              ^ ^                    ^ ^                 ^ ^          │ Git │
-- ╭^─^──────────────^─^────────────────────^─^─────────────────^─^──────────┴─────╯
--  _J_: next hunk   _s_: stage hunk        _d_: show deleted   _b_: blame line
--  _K_: prev hunk   _u_: undo stage hunk   _p_: preview hunk   _B_: blame show full 
--  ^ ^              _S_: stage buffer      ^ ^                 _/_: show base file
--  ^
--  ^ ^              _<Enter>_: Neogit             _q_: exit
-- ]]

local hint_vert = [[
 _J_: next hunk
 _K_: prev hunk
 ^
 _s_: stage hunk
 _u_: undo last stage 
 _S_: stage buffer
 ^
 _d_: show deleted
 _p_: preview hunk
 ^
 _b_: blame line
 _B_: blame show full
 _/_: show base file
 ^
 _<Enter>_: Neogit
       _q_: exit
]]

local git = {
   name = 'Git',
   hint = hint,
   -- hint = hint_2,
   -- hint = hint_vert,
   config = {
      debug = true,
      color = 'pink',
      invoke_on_body = true,
      hint = {
         -- position = 'top-right',
         -- position = 'bottom-right',
         -- position = 'middle-right',
         border = 'rounded'
      },
      on_enter = function()
         vim.bo.modifiable = false
         gitsigns.toggle_signs(true)
         gitsigns.toggle_linehl(true)
      end,
      on_exit = function()
         gitsigns.toggle_signs(false)
         gitsigns.toggle_linehl(false)
         gitsigns.toggle_deleted(false)
         -- vim.cmd 'echo' -- clear the echo area
      end,
      -- timeout = 3000
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

      { 'b', gitsigns.blame_line, { desc = 'blame' } },
      { 'B', function() gitsigns.blame_line{ full = true } end,
         { desc = 'blame show full' } },

      { '/', gitsigns.show, { exit = true, desc = 'show base file' } }, -- show the base of the file

      { '<Enter>', '<cmd>Neogit<CR>', { exit = 'after', desc = 'Neogit' } },

      { 'q', nil, { exit = true, nowait = true, desc = 'exit' } },
      -- { '<Esc>', nil, { exit = true, desc = 'exit' } }
   }
}

Hydra(git)
