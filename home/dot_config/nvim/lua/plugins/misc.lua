return {
   {
      "HiPhish/rainbow-delimiters.nvim",
      config = function()
         require('rainbow-delimiters.setup').setup({
            highlight = {
               'RainbowDelimiterViolet',
               'RainbowDelimiterBlue',
               'RainbowDelimiterOrange',
               'RainbowDelimiterGreen',
               'RainbowDelimiterYellow',
               'RainbowDelimiterCyan',
               'RainbowDelimiterRed',
            },
         })
      end
   },
   {
      "echasnovski/mini.trailspace",
      version = "*",
      config = function()
         require('mini.trailspace').setup()
         vim.api.nvim_create_autocmd("BufWrite", {
            callback = function()
               MiniTrailspace.trim()
               MiniTrailspace.trim_last_lines()
            end
         })
      end
   },
   {
      "catgoose/nvim-colorizer.lua",
      event = "BufReadPre",
      ft = { "vim", "lua", "conf", "tmux", "kitty", "vifm", "markdown", "zsh" },
      opts = {},
   },
   {
      "tummetott/unimpaired.nvim",
      event = "VeryLazy",
      opts = {
         default_keymaps = false,
         keymaps = {
            blank_above = {
               mapping = "[<Space>",
               description = "Add blank line above",
               dot_repeat = true,
            },
            blank_below = {
               mapping = "]<Space>",
               description = "Add blank line below",
               dot_repeat = true,
            },
         },
      },
   }
}
