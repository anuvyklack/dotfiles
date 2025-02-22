return {
   {
      "karb94/neoscroll.nvim",
      opts = {
         -- Default:
         -- { "<C-u>", "<C-d>", "<C-b>", "<C-f>", "<C-y>", "<C-e>", "zt", "zz", "zb" },
         mappings = { "zt", "zz", "zb" },

         hide_cursor = true, -- Hide cursor while scrolling
         stop_eof = true, -- Stop at <EOF> when scrolling downwards

         -- Stop scrolling when the cursor reaches the scrolloff margin of the file.
         respect_scrolloff = false,

         -- Use the local scope of scrolloff instead of the global scope.
         use_local_scrolloff = false,

         -- The cursor will keep on scrolling even if the window cannot scroll further.
         cursor_scrolls_alone = false,

         -- easing_function = nil, -- Default
         easing_function = "quadratic",
         -- easing_function = "cubic",
         -- easing_function = "quartic",
         -- easing_function = "quintic",
         -- easing_function = "circular",
         -- easing_function = "sine",

         pre_hook = nil, -- Function to run before the scrolling animation starts
         post_hook = nil, -- Function to run after the scrolling animation ends
         performance_mode = false, -- Disable "Performance Mode" on all buffers.
      },
      keys = {
         {
            "<C-u>", function()
               require("neoscroll").scroll(1 - vim.wo.scroll, {
                  move_cursor = false,
                  duration = 250,
               })
            end,
            mode = {"n", "v"}
         },
         {
            "<C-d>", function()
               require("neoscroll").scroll(vim.wo.scroll - 1, {
                  move_cursor = false,
                  duration = 250,
               })
            end,
            mode = {"n", "v"}
         },
         {
            "<C-b>", function()
               require("neoscroll").scroll(-vim.api.nvim_win_get_height(0), {
                  move_cursor = false,
                  duration = 450,
               })
            end,
            mode = {"n", "v"}
         },
         {
            "<C-f>", function()
               require("neoscroll").scroll(vim.api.nvim_win_get_height(0), {
                  move_cursor = false,
                  duration = 450,
               })
            end,
            mode = {"n", "v"}
         },
      },
      -- config = function()
      --    -- Syntax: [mapping] = { function, { function arguments } }
      --    -- Scrolling value can be an indeger (the number of lines) or fraction
      --    -- (0.1 = 10%) of screen height.
      --    require("neoscroll.config").set_mappings({
      --       -- ['<C-y>'] = { 'scroll', { '-3', 'false', '100' } },
      --       -- ['<C-e>'] = { 'scroll', {  '3', 'false', '100' } },
      --       -- ['zt']    = { 'zt', { '250' } },
      --       -- ['zz']    = { 'zz', { '250' } },
      --       -- ['zb']    = { 'zb', { '250' } },
      --    })
      -- end,
   },
}
