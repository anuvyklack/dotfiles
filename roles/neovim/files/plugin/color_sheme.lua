--                 ███                                    ██
--                ░░██                                   ░██
--   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
--  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
-- ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
-- ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
-- ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
--  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░

local g, o, cmd = vim.g, vim.o, vim.cmd
local function colorscheme(scheme)
   cmd(table.concat({
      'try',
         'colorscheme '..scheme,
      [[catch /^Vim\%((\a\+)\)\=:E185:/]],
      'endtry'
   }, '\n'))
end

local theme = { 'gruvbox-material', 'dark' }
-- local theme = { 'gruvbox-material', 'light' }
-- local theme = 'tokyonight'
-- local theme = { 'melange', 'light' }
-- local theme = { 'melange', 'dark' }
-- local theme = { 'mellow', 'light' }
-- local theme = { 'mellow', 'dark' }
-- local theme = 'moonshine'
-- local theme = 'moonshine_lowcontrast'
-- local theme = 'moonshine_minimal'
-- local theme = 'srcery'
-- local theme = { 'everforest', 'light' }
-- local theme = 'manuscript'

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
      g.gruvbox_material_cursor = 'aqua'

      -- 'colored' or 'grey'
      g.gruvbox_material_diagnostic_virtual_text = 'colored'
      g.gruvbox_material_diagnostic_text_highlight = 1 -- 0 or 1

      -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
      g.gruvbox_material_current_word = 'grey background'
      g.gruvbox_material_better_performance = 1

      colorscheme 'gruvbox-material'
      cmd 'source ~/.config/nvim/lua/anuvyklack/gruvbox-material.vim'
   end,
   mellow = function()
      colorscheme 'mellow'
      cmd 'source ~/.config/nvim/lua/plugins-config/mellow.vim'
   end,
   everforest = function()
      g.everforest_background = 'soft'
      g.everforest_better_performance = 1
      colorscheme 'everforest'
   end,
}

local flavor; if type(theme) == 'table' then
   theme, flavor = theme[1], theme[2]
end
if flavor then o.background = flavor end
if color_themes[theme] then
   color_themes[theme]()
else
   colorscheme(theme)
end

-- vim: fdm=manual fml=1
