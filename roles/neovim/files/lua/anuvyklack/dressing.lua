require('dressing').setup {
   input = {
      insert_only = false, -- Close input window on exit insert mode.
      win_options = {
         winblend = 0, -- Window transparency (0-100)
         wrap = false, -- Disable line wrapping
         winhighlight = 'Normal:Normal,FloatBorder:Grey'
      },
   },
   -- select = {
   --    -- Priority list of preferred vim.select implementations
   --    backend = { --[['builtin',]] 'nui', 'telescope', 'fzf_lua', 'fzf' },
   --
   --    nui = { -- Options for nui Menu
   --       -- position = "50%",
   --       -- size = nil,
   --       -- relative = "editor",
   --       -- border = {
   --       --   style = "rounded",
   --       -- },
   --       -- buf_options = {
   --       --   swapfile = false,
   --       --   filetype = "DressingSelect",
   --       -- },
   --       win_options = {
   --         winblend = 0,
   --       },
   --       -- max_width = 80,
   --       -- max_height = 40,
   --       -- min_width = 40,
   --       -- min_height = 10,
   --    },
   --
   --    builtin = { -- Options for built-in selector
   --       -- anchor = 'NW',
   --       -- border = 'rounded',
   --       relative = 'cursor', -- 'editor' and 'win' will default to being centered
   --
   --       winblend = 0, -- Window transparency (0-100)
   --
   --       winhighlight = 'Normal:Normal,FloatBorder:Normal,CursorLine:Visual,Search:None',
   --
   --       -- These can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
   --       -- the min_ and max_ options can be a list of mixed types.
   --       -- max_width = {140, 0.8} means "the lesser of 140 columns or 80% of total"
   --       width = nil,
   --       max_width = { 140, 0.8 },
   --       min_width = { 40, 0.2 },
   --       height = nil,
   --       max_height = 0.9,
   --       min_height = 3
   --    }
   -- }
}
