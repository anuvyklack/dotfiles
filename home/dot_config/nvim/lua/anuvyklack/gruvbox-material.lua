local function hl(name, spec)
   vim.api.nvim_set_hl(0, name, spec)
end

local function init()
   local g = vim.g

   -- Set contrast. Available values: 'hard', 'medium'(default), 'soft'
   if vim.o.background == "dark" then
      g.gruvbox_material_background = "medium"
   else -- light
      g.gruvbox_material_background = "soft"
   end

   -- Set the color palette used in this color scheme.
   -- material : material palette with soft contrast;
   -- mix      : the mean of the other two;
   -- original : the original gruvbox palette.
   g.gruvbox_material_palette = "mix"

   g.gruvbox_material_enable_bold = 1
   g.gruvbox_material_enable_italic = 1

   -- Available values: 'auto', 'red', 'orange', 'yellow',
   -- 'green', 'aqua', 'blue', 'purple'
   g.gruvbox_material_cursor = "yellow"

   -- 'colored' or 'grey'
   g.gruvbox_material_diagnostic_virtual_text = "colored"
   g.gruvbox_material_diagnostic_text_highlight = 1 -- 0 or 1

   -- -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
   -- g.gruvbox_material_current_word = 'grey background'
   -- g.gruvbox_material_better_performance = 1

   -- require('anuvyklack/gruvbox-material')
   -- vim.cmd 'source ~/.config/nvim/lua/anuvyklack/gruvbox-material.vim'
end

local function config()
   vim.cmd("colorscheme gruvbox-material")

   local colors
   do
      --                        black             #282828
      --   fg0        #e2cca9   bg_dim            #1b1b1b
      --   fg1        #e2cca9   bg0               #282828
      --                        bg1               #32302f
      --   grey0      #7c6f64   bg2               #32302f
      --   grey1      #928374   bg3               #45403d
      --   grey2      #a89984   bg4               #45403d
      --                        bg5               #5a524c
      --   aqua       #8bba7f   bg_current_word   #3c3836
      --   blue       #80aa9e
      --   green      #b0b846   bg_diff_blue      #0e363e
      --   orange     #f28534   bg_diff_green     #34381b
      --   purple     #d3869b   bg_diff_red       #402120
      --   red        #f2594b
      --   yellow     #e9b143   bg_statusline1    #32302f
      --                        bg_statusline2    #3a3735
      --                        bg_statusline3    #504945
      --
      --   bg_yellow  #e9b143   bg_visual_blue    #374141
      --   bg_red     #db4740   bg_visual_green   #3b4439
      --   bg_green   #b0b846   bg_visual_red     #4c3432
      --                        bg_visual_yellow  #4f422e

      local config = vim.fn["gruvbox_material#get_configuration"]()
      local get_palette = vim.fn["gruvbox_material#get_palette"]
      colors = get_palette(config.background, config.foreground, config.colors_override)
      for key, color in pairs(colors) do
         colors[key] = color[1]
      end
   end

   hl('ErrorMsg', { fg = '#f26458', bold = true })

   do -- Color brackets
      -- '#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'
      -- '#f28534', '#80aa9e', '#e9b143', '#b0b846', '#d3869b', '#f2594b'
      hl('RainbowDelimiterViolet', { fg = '#c475c1' })
      hl('RainbowDelimiterBlue', { fg = '#8ab7d8' })
      -- hl('RainbowDelimiterOrange', { fg = '#f28534' })
      hl('RainbowDelimiterGreen', { fg = '#98c369' })
      hl('RainbowDelimiterYellow', { fg = '#e9b143' })
      -- 'RainbowDelimiterCyan'
      hl('RainbowDelimiterRed', { fg = '#f2594b' })
   end

   do -- easymotion
      -- highlight HopNextKey   guifg=#ff1414 gui=bold
      -- highlight HopNextKey1  guifg=#ffb912 gui=bold
      -- highlight HopNextKey2  guifg=#e3a84e
      hl('HopNextKey',  { fg = '#ff1414', bold = true })
      hl('HopNextKey1', { fg = '#ffb912', bold = true })
      hl('HopNextKey2', { fg = '#e3a84e' })

      -- hl('FlashLabel', { fg = '#ffb912', bg = nil, bold = true })
   end

   hl('MiniTrailspace', { link = 'Orange' })

   local config_path = vim.fn.stdpath("config") --[[@as string]]
   vim.cmd("source" .. vim.fs.joinpath(config_path, "lua", "anuvyklack", "gruvbox-material.vim"))
end

return {
   "sainnhe/gruvbox-material",
   init = init,
   config = config,
   priority = 1000,
   lazy = false
}
