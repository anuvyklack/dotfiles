local hydra = require('hydra')

hydra({
   name = "OnTest",
   config = {
      debug = true,
      color = 'amaranth',

      invoke_on_body = true,
      on_enter = function()
        vim.bo.modifiable = false
      end,
      on_exit = function()
        vim.cmd 'echo'
      end
   },
   mode = 'n',
   body = '<leader>t',
   heads = {
      {'r', nil, { exit = true }}
   }
})
