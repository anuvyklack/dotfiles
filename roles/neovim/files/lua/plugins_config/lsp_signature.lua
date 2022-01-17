require('lsp_signature').setup {
   ---Will show two lines of comment/doc(if there are more than two lines in
   ---doc, will be truncated).
   ---Set to 0 if you DO NOT want any API comments be shown.
   ---This setting only take effect in insert mode, it does not affect
   ---signature help in normal mode, 10 by default
   doc_lines = 0,

   ---Show hint in a floating window, set to false for virtual text only mode.
   floating_window = true,

   -- Try to place the floating above the current line when possible.
   -- Set to false will use whichever side has more space.  This setting will
   -- be helpful if you do not want the PUM and floating win overlap.
   floating_window_above_cur_line = true,

   hint_enable = true,  -- virtual hint enable
   hint_prefix = "🐼 ", -- Panda for parameter
   hint_scheme = "String",
   hi_parameter = 'LspSignatureActiveParameter',
   handler_opts = {
      border = 'shadow'   -- double, rounded, single, shadow, none
   },

   -- Array of extra characters that will trigger signature completion.
   -- e.g., {"(", ","}
   extra_trigger_chars = {}
}
