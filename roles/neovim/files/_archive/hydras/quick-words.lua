local Hydra = require('hydra')

-- vim.keymap.set({'n','x','o'}, 'w',  '<plug>WordMotion_w')
-- vim.keymap.set({'n','x','o'}, 'b',  '<plug>WordMotion_b')
-- vim.keymap.set({'n','x','o'}, 'e',  '<plug>WordMotion_e')
-- vim.keymap.set({'n','x','o'}, 'ge', '<plug>WordMotion_ge')

Hydra({
   name = 'Quick words',
   config = {
      -- debug = true,
      color = 'pink',
      hint = false
      -- hint = 'statusline'
   },
   mode = {'n','x','o'},
   body = ',',
   heads = {
      -- { 'w',  '<Plug>(quickword-w)' },
      -- { 'b',  '<Plug>(quickword-b)' },
      -- { 'e',  '<Plug>(quickword-e)' },
      -- { 'ge', '<Plug>(quickword-ge)' },

      { 'w',  '<Plug>(smartword-w)' },
      { 'b',  '<Plug>(smartword-b)' },
      { 'e',  '<Plug>(smartword-e)' },
      { 'ge', '<Plug>(smartword-ge)' },

      { '<Esc>', nil, { exit = true, mode = 'n' } }
   }
})
