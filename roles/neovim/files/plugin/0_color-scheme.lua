--                 ███                                    ██
--                ░░██                                   ░██
--   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
--  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
-- ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
-- ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
-- ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
--  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░
--
-- Should be loaded before all other plugins, to allow them to use color scheme highlight groups.

local g, o, cmd = vim.g, vim.o, vim.cmd

local function colorscheme(scheme)
   cmd(table.concat({
      'try',
      'colorscheme ' .. scheme,
      [[catch /^Vim\%((\a\+)\)\=:E185:/]],
      'endtry'
   }, '\n'))
end

local function hl(name, spec)
   vim.api.nvim_set_hl(0, name, spec)
end

-- local theme = { 'gruvbox-material', 'dark' }
-- local theme = { 'gruvbox-material', 'light' }
-- local theme = 'tokyonight'
-- local theme = { 'melange', 'light' }
-- local theme = { 'melange', 'dark' }
-- local theme = { 'mellow', 'light' }
-- local theme = { 'mellow', 'dark' }
-- local theme = 'srcery'
-- local theme = { 'everforest', 'dark' }
-- local theme = { 'everforest', 'light' }
-- local theme = { "zenbones", "light" }
-- local theme = { "zenbones", "dark" }
-- local theme = { "onedark", "dark" }
-- local theme = { "onedard", "light"}
-- local theme = 'manuscript'
local theme = 'blasphemous'
-- local theme = "dayfox"

local color_themes = {
   ['gruvbox-material'] = function()
      local background = o.background

      -- Set contrast. Available values: 'hard', 'medium'(default), 'soft'
      if background == 'dark' then
         g.gruvbox_material_background = 'medium'
      else -- light
         g.gruvbox_material_background = 'soft'
      end

      -- Set the color palette used in this color scheme.
      -- material : material palette with soft contrast;
      -- mix      : the mean of the other two;
      -- original : the original gruvbox palette.
      g.gruvbox_material_palette = 'mix'

      g.gruvbox_material_enable_bold = 1
      g.gruvbox_material_enable_italic = 1

      -- Available values: 'auto', 'red', 'orange', 'yellow',
      -- 'green', 'aqua', 'blue', 'purple'
      g.gruvbox_material_cursor = 'yellow'

      -- 'colored' or 'grey'
      g.gruvbox_material_diagnostic_virtual_text = 'colored'
      g.gruvbox_material_diagnostic_text_highlight = 1 -- 0 or 1

      -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
      g.gruvbox_material_current_word = 'grey background'
      g.gruvbox_material_better_performance = 1

      colorscheme 'gruvbox-material'
      require('anuvyklack/gruvbox-material')
      cmd 'source ~/.config/nvim/lua/anuvyklack/gruvbox-material.vim'
   end,
   mellow = function()
      colorscheme 'mellow'
      cmd 'source ~/.config/nvim/lua/anuvyklack/mellow.vim'
   end,
   everforest = function()
      g.everforest_background = 'soft'
      g.everforest_better_performance = 1
      colorscheme 'everforest'
   end,
   tokyonight = function()
      require("tokyonight").setup({
         style = "storm",
         dim_inactive = true, -- dims inactive windows
         on_colors = function(colors)
            colors.comment = "#868eb6"
         end,
         on_highlights = function(hl, c)
            hl.MiniTrailspace = { fg = c.orange }
            hl.NormalFloat = { bg = "#373d58" }
            hl.FloatBorder = { fg = hl.FloatBorder.fg, bg = "#373d58" }
            hl.Function = { fg = "#8cafff", style = { bold = true } }
            hl.FoldColumn = { fg = hl.FoldColumn.fg, bg = "#343953" }
            hl.SignColumn = { fg = hl.SignColumn.fg, bg = "#343953" }
            hl.WinSeparator = { fg = "#868eb6" }
         end
      })
      require('anuvyklack/tokyonight')

      colorscheme "tokyonight"
   end,
   zenbones = function()
      -- vim.g.zenbones = {
      --    lightness = 'dim'
      -- }
      colorscheme "zenbones"

      hl("Cursor", { fg = "#ffffff", bg = "#8cafff" })
      -- hl("Cursor", { reverse = true })
   end,
   rose_pine = function()
      require("rose-pine").setup()
      colorscheme 'rose-pine'
   end,
   onedark = function()
      local black   = "#282c34"
      local white   = "#abb2bf"
      local red     = "#e06c75"
      local red2    = "#be5046"
      local green   = "#98c379"
      local yellow  = "#e5c07b"
      local yellow2 = "#d19a66"
      local blue    = "#61afef"
      local magenta = "#c678dd"
      local cyan    = "#56b6c2"
      local grey1   = "#4b5263"
      local grey2   = "#5c6370"
      require("onedark").setup {
         -- style = "warmer"
         diagnostics = {
            background = true, -- use background color for virtual text
         },
         highlights = {
            -- Normal = { fg = "#abb2bf", bg = "#31353f" },
            Comment = { fg = "#6f7787" },
            ["@comment"] = { fg = "#6f7787" },
            WinSeparator = { fg = "#757d8b" , bg = "#31353f" },
            NormalFloat = { bg = "#353943" },
            FloatBorder = { bg = "#353943" },
            -- Search = { fg = "#282c34", bg = "#ebd09c" }
            Search = { fg = "#282c34", bg = "#8c8c8c", bold = true },
            CurSearch = { fg = "#282c34", bg = "#d19a66" },
            -- Blueflower
            -- ["@bf.H1"] = { fg = red, fmt = "bold" },
            -- ["@bf.H2"] = { fg = yellow2, fmt = "bold" },
            -- ["@bf.H3"] = { fg = blue, fmt = "bold" },
            -- ["@bf.H4"] = { fg = green, fmt = "bold" },
            -- ["@bf.H5"] = { fg = magenta, fmt = "bold" },
            -- ["@bf.H6"] = { fg = cyan, fmt = "bold" },
            -- ["@bf.verbatim"] = { fg = blue },
         }
      }
      require("onedark").load()
   end,
}

local flavor;
if type(theme) == 'table' then
   theme, flavor = theme[1], theme[2]
end
if flavor then o.background = flavor end
if color_themes[theme] then
   color_themes[theme]()
else
   colorscheme(theme)
end

-- vim.api.nvim_set_hl(0, 'Cursor', { fg = "blue", bg = "blue"})
-- vim.api.nvim_set_hl(0, 'Cursor2', { fg = "red", bg = "red"})

vim.opt.guicursor = {
   "n-v-c:block-Cursor",
   "i-ci-ve:ver25-Cursor",
   "r-cr:hor20-Cursor",
   "o:hor50-Cursor"
}

-- vim: fdm=manual fml=1
