--                 ███                                    ██
--                ░░██                                   ░██
--   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
--  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
-- ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
-- ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
-- ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
--  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░

local colorscheme = { 'gruvbox-material', 'dark' }
-- local colorscheme = { 'gruvbox-material', 'light' }
-- local colorscheme = 'tokyonight'
-- local colorscheme = { 'melange', 'light' }
-- local colorscheme = { 'melange', 'dark' }
-- local colorscheme = { 'mellow', 'light' }
-- local colorscheme = { 'mellow', 'dark' }
-- local colorscheme = 'moonshine'
-- local colorscheme = 'moonshine_lowcontrast'
-- local colorscheme = 'moonshine_minimal'
-- local colorscheme = 'srcery'
-- local colorscheme = { 'everforest', 'light }
-- local colorscheme = 'manuscript'

local color_themes = {
   ['gruvbox-material'] = function()
         local background = vim.o.background

         -- Set contrast. Available values: 'hard', 'medium'(default), 'soft'
         if background == 'dark' then
            vim.g.gruvbox_material_background = 'medium'
         else -- light
            vim.g.gruvbox_material_background = 'soft'
         end

         -- Set the color palette used in this color scheme.
         -- material : material palette with soft contrast;
         -- mix      : the mean of the other two;
         -- original : the original gruvbox palette.
         vim.g.gruvbox_material_palette = 'mix'

         vim.g.gruvbox_material_enable_bold = 1
         vim.g.gruvbox_material_enable_italic = 1

         -- Available values: 'auto', 'red', 'orange', 'yellow',
         -- 'green', 'aqua', 'blue', 'purple'
         vim.g.gruvbox_material_cursor = 'aqua'

         -- 'colored' or 'grey'
         vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
         vim.g.gruvbox_material_diagnostic_text_highlight = 1 -- 0 or 1

         -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
         vim.g.gruvbox_material_current_word = 'grey background'
         vim.g.gruvbox_material_better_performance = 1

         vim.cmd 'colorscheme gruvbox-material'
         vim.cmd 'source ~/.config/nvim/lua/anuvyklack/gruvbox-material.vim'
   end,
   mellow = function()
      vim.cmd 'colorscheme mellow'
      vim.cmd 'source ~/.config/nvim/lua/plugins-config/mellow.vim'
   end,
   everforest = function()
      vim.g.everforest_background = 'soft'
      vim.g.everforest_better_performance = 1
      vim.cmd 'colorscheme everforest'
   end,
}

local flavor; if type(colorscheme) == 'table' then
   colorscheme, flavor = colorscheme[1], colorscheme[2]
end
if flavor then
   vim.o.background = flavor
end
if color_themes[colorscheme] then
   color_themes[colorscheme]()
else
   vim.cmd('colorscheme '..colorscheme)
end

-- vim: fdm=manual fml=1
