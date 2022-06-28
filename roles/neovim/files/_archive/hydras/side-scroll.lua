local Hydra = require('hydra')

Hydra({
   name = 'Side scroll',
   config = {
      timeout = 2000,
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

