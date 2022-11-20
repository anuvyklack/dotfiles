require('which-key').setup {
   plugins = {
      -- Enabling this module will show WhichKey when pressing z=
      -- to select spelling suggestions.
      spelling = {
         enabled = false,
         suggestions = 20, -- How many suggestions should be shown in the list?
      },
      presets = {
         operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
         motions = true, -- adds help for motions
         text_objects = true, -- help for text objects triggered after entering an operator
         windows = true, -- default bindings on <c-w>
         nav = true, -- misc bindings to work with windows
         z = true, -- bindings for folds, spelling and others prefixed with z
         g = true, -- bindings for prefixed with g
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
