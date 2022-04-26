require'nvim-lightbulb'.setup({
   sign = {
      enabled = true,
      priority = 10, -- Priority of the gutter sign
   },
   virtual_text = {
      enabled = false,
      text = "",
      -- highlight mode to use for virtual text (replace, combine,
      -- blend), see :help nvim_buf_set_extmark() for reference
      hl_mode = "replace",
   },
})

vim.fn.sign_define('LightBulbSign', {
   text = "", --  
   texthl = "LspDiagnosticsInformation",
   linehl = "LspDiagnosticsInformation",
   numhl  = "LspDiagnosticsInformation"
})

vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
   callback = require'nvim-lightbulb'.update_lightbulb
})
