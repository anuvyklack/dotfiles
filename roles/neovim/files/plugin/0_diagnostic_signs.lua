-- This file should be loaded before "1_packer_compiled.lua" since it defines
-- diagnostic signs. Otherwise, heirline statusline wouldn't be loaded.

local sign = vim.fn.sign_define

--       󰂭
--     
--  
--     
--     
--    
sign('DiagnosticSignError', { text = "", texthl = 'DiagnosticSignError' })
sign('DiagnosticSignWarn',  { text = "", texthl = 'DiagnosticSignWarn'  })
sign('DiagnosticSignInfo',  { text = "", texthl = 'DiagnosticSignInfo'  })
sign('DiagnosticSignHint',  { text = "",  texthl = 'DiagnosticSignHint'  })

-- vim: fml=1
