require('lsp_signature').setup {
   doc_lines = 0,

   ---Show hint in a floating window, set to "false" for virtual text only mode.
   -- floating_window = true,
   floating_window = false,

   hint_enable = true,  -- virtual hint enable
   hint_prefix = "🐼 ", -- Panda for parameter
   -- hint_scheme = 'String',
   hint_scheme = 'Comment',
   hi_parameter = 'LspSignatureActiveParameter',
   handler_opts = {
      border = 'shadow'   -- double, rounded, single, shadow, none
   },

   -- Array of extra characters that will trigger signature completion.
   -- e.g., { '(', ',' }
   extra_trigger_chars = {}
}
