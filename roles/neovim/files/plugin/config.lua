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