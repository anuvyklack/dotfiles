-- Colors            {{{
--
-- fg0        #e2cca9   bg0               #32302f
-- fg1        #e2cca9   bg1               #3c3836
--                      bg2               #3c3836
-- grey0      #7c6f64   bg3               #504945
-- grey1      #928374   bg4               #504945
-- grey2      #a89984   bg5               #665c54
--                      bg_current_word   #45403d
-- aqua       #8bba7f   bg_diff_blue      #0f3a42
-- blue       #80aa9e   bg_diff_green     #3d4220
-- green      #b0b846   bg_diff_red       #472322
-- orange     #f28534
-- purple     #d3869b   bg_statusline1    #3c3836
-- red        #f2594b   bg_statusline2    #46413e
-- yellow     #e9b143   bg_statusline3    #5b534d
--                      bg_visual_blue    #404946
-- bg_yellow  #e9b143   bg_visual_green   #424a3e
-- bg_green   #b0b846   bg_visual_red     #543937
-- bg_red     #db4740   bg_visual_yellow  #574833
--
-- let config = gruvbox_material#get_configuration()
-- let palette = gruvbox_material#get_palette(&background, config.palette)
-- echo palette
--
-- }}}

local function hl(name, val)
   vim.api.nvim_set_hl(0, name, val)
end

-- hl('HydraRed',  { fg = '#FF5733', bold = true })
--
-- hl('HydraHint', { link = 'NormalFloat' })
--
-- hl('StatusLine',   { fg = '#e2cca9', bg = '#3c3836' })
-- hl('WinSeparator', { fg = '#928374' })


-- Color brackets	{{{
-- '#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'
-- '#f28534', '#f2594b', '#80aa9e', '#e9b143', '#b0b846', '#d3869b'
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
hl('@neorg.markup.verbatim', { link = 'Green' })

-- code blocks
-- hl('@neorg.tags.ranged_verbatim.begin',     { fg = '#5a5a5a' })
hl('@neorg.tags.ranged_verbatim.begin',     { fg = '#808080' })
hl('@neorg.tags.ranged_verbatim.name.word', { fg = '#808080' })
hl('@neorg.tags.ranged_verbatim.end',       { fg = '#808080' })

-- }}}

-- vim: fdm=marker

