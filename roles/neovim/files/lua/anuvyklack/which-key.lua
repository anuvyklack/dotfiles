require('which-key').setup {
   plugins = {
      -- Enabling this module will show WhichKey when pressing z=
      -- to select spelling suggestions.
      spelling = {
         enabled = false,
         suggestions = 20, -- How many suggestions should be shown in the list?
      },
      presets = {
         operators = true,
      },
   },
   operators = {
      gc = "Comments"
   },
   icons = {
      breadcrumb = "»",
      separator = "", -- U+279c: ➜ (Unicode Heavy Round-Tipped Rightwards Arrow)
      group = "+",
   },
   -- Enable this to hide mappings for which you didn't specify a label.
   -- ignore_missing = true,
   hidden = { -- hide mapping boilerplate
      "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ",
      "<SNR>", "<Plug>"
   },
}
