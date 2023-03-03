if not pcall(require, 'api-wrappers/highlight') then return end
local H = require('api-wrappers/highlight')
local set_hl = H.set_highlight
local get_hl = H.get_highlight

local colors
do --{{{
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

   local config = vim.fn['gruvbox_material#get_configuration']()
   local get_palette = vim.fn['gruvbox_material#get_palette']
   colors = get_palette(config.background, config.foreground, config.colors_override)
   for key, color in pairs(colors) do
      colors[key] = color[1]
   end
end --}}}

-- hl('HydraRed',  { fg = '#FF5733', bold = true })
--
-- hl('HydraHint', { link = 'NormalFloat' })
--
-- hl('StatusLine',   { fg = '#e2cca9', bg = '#3c3836' })
-- hl('WinSeparator', { fg = '#928374' })

-- set_hl("@punctuation.delimiter", { link = "Normal" })

set_hl('ErrorMsg', { fg = '#f26458', bold = true })

-- QuickFix {{{
set_hl('QuickFixLine', { bg ='#40383c', bold = true })

set_hl('qfError', { link = 'DiagnosticSignError' })
set_hl('qfWarn',  { link = 'DiagnosticSignWarn' })
set_hl('qfHint',  { link = 'DiagnosticSignHint' })
set_hl('qfInfo',  { link = 'DiagnosticSignInfo' })
--}}}

-- Diff {{{
set_hl('DiffText', { bg='#174f59' })
-- hl('DiffText', { fg='#e2cca9', bg='#1a545f' })
--}}}

-- Color brackets	{{{
-- '#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'
-- '#f28534', '#80aa9e', '#e9b143', '#b0b846', '#d3869b', '#f2594b'
prequire('nvim-treesitter.configs').setup {
   rainbow = {
	  colors = {
		 '#f28534', '#80aa9e', '#e9b143', '#b0b846', '#d3869b', '#f2594b'
	  },
   },
}
--}}}

-- Neorg {{{
-- hl('@neorg.markup.verbatim', { link = 'Blue' })
set_hl('@neorg.markup.verbatim', { link = 'Green' })

-- code blocks
-- hl('@neorg.tags.ranged_verbatim.begin',     { fg = '#5a5a5a' })
set_hl('@neorg.tags.ranged_verbatim.begin',     { fg = '#808080' })
set_hl('@neorg.tags.ranged_verbatim.name.word', { fg = '#808080' })
set_hl('@neorg.tags.ranged_verbatim.end',       { fg = '#808080' })
--}}}

-- Skald {{{

set_hl('@bf.heading1', { link = 'RedBold' })
set_hl('@bf.heading2', { link = 'OrangeBold' })
set_hl('@bf.heading3', { link = 'YellowBold' })
set_hl('@bf.heading4', { link = 'GreenBold' })
set_hl('@bf.heading5', { link = 'BlueBold' })
set_hl('@bf.heading6', { link = 'PurpleBold' })

-- set_hl('markdownUrl', s:palette.blue, s:palette.none, 'underline')

-- }}}

-- Barbar {{{
set_hl('BufferCurrent',        { fg = colors.fg1,    bg = colors.bg5 })
set_hl('BufferCurrentIndex',   { fg = colors.fg1,    bg = colors.bg5 })
set_hl('BufferCurrentMod',     { fg = colors.blue,   bg = colors.bg5 })
set_hl('BufferCurrentSign',    { fg = colors.grey2,  bg = colors.bg5 })
set_hl('BufferCurrentTarget',  { fg = colors.red,    bg = colors.bg5,   bold = true })
set_hl('BufferVisible',        { fg = colors.fg1,    bg = colors.bg3 })
set_hl('BufferVisibleIndex',   { fg = colors.fg1,    bg = colors.bg3 })
set_hl('BufferVisibleMod',     { fg = colors.blue,   bg = colors.bg3 })
set_hl('BufferVisibleSign',    { fg = colors.grey2,  bg = colors.bg3 })
set_hl('BufferVisibleTarget',  { fg = colors.yellow, bg = colors.bg3,   bold = true })
set_hl('BufferInactive',       { fg = colors.grey1,  bg = colors.bg3 })
set_hl('BufferInactiveIndex',  { fg = colors.grey1,  bg = colors.bg3 })
set_hl('BufferInactiveMod',    { fg = colors.grey1,  bg = colors.bg3 })
set_hl('BufferInactiveSign',   { fg = colors.grey0,  bg = colors.bg3 })
set_hl('BufferInactiveTarget', { fg = colors.yellow, bg = colors.bg3,   bold = true })
set_hl('BufferTabpages',       { fg = colors.bg0,    bg = colors.grey2, bold = true })
set_hl('BufferTabpageFill',    { fg = colors.bg0,    bg = colors.bg0 })
-- }}}

-- Mini {{{
set_hl('MiniTrailspace', { link = 'Orange' })
-- }}}

-- vim: fdm=marker

