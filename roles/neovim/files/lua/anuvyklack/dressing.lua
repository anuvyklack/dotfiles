require('dressing').setup {
   input = {
      insert_only = false, -- When true, <Esc> will close the modal.

      -- These are passed to nvim_open_win
      anchor = "NW",

      winblend = 0, -- Window transparency (0-100)
      winhighlight = 'Normal:Normal,FloatBorder:Grey'
   },
   select = {
      -- Priority list of preferred vim.select implementations
      backend = { 'builtin', 'nui', 'telescope', 'fzf_lua', 'fzf' },

      -- Options for built-in selector
      builtin = {
         -- These are passed to nvim_open_win
         anchor = 'NW',
         border = 'rounded',

         -- 'editor' and 'win' will default to being centered
         relative = 'cursor',

         winblend = 0, -- Window transparency (0-100)

         -- Change default highlight groups (see :help winhl)
         winhighlight = 'Normal:Normal,FloatBorder:Normal,CursorLine:Visual,Search:None',

         -- These can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
         -- the min_ and max_ options can be a list of mixed types.
         -- max_width = {140, 0.8} means "the lesser of 140 columns or 80% of total"
         width = nil,
         max_width = { 140, 0.8 },
         min_width = { 40, 0.2 },
         height = nil,
         max_height = 0.9,
         min_height = 3,
      }
   }
}
