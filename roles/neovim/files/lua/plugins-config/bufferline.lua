-- grey0   #7c6f64    bg0               #282828
-- grey1   #928374    bg1               #32302f
-- grey2   #a89984    bg3               #45403d
--                    bg5               #5a524c
-- fg0     #e2cca9
-- red     #f2594b    bg_statusline1    #32302f
-- orange  #f28534    bg_statusline2    #3a3735
-- yellow  #e9b143    bg_statusline3    #504945
-- green   #b0b846
-- aqua    #8bba7f    bg_diff_green     #34381b
-- blue    #80aa9e    bg_visual_green   #3b4439
-- purple  #d3869b    bg_diff_red       #402120
-- bg_red  #db4740    bg_visual_red     #4c3432
--                    bg_diff_blue      #0e363e
--                    bg_visual_blue    #374141
--                    bg_visual_yellow  #4f422e
--                    bg_current_word   #3c3836

-- BufferCurrent         s:palette.fg1     s:palette.bg5
-- BufferCurrentIndex    s:palette.fg1     s:palette.bg5
-- BufferCurrentMod      s:palette.blue    s:palette.bg5
-- BufferCurrentSign     s:palette.grey2   s:palette.bg5
-- BufferCurrentTarget   s:palette.red     s:palette.bg5   'bold'

-- BufferVisible         s:palette.fg1     s:palette.bg3
-- BufferVisibleIndex    s:palette.fg1     s:palette.bg3
-- BufferVisibleMod      s:palette.blue    s:palette.bg3
-- BufferVisibleSign     s:palette.grey2   s:palette.bg3
-- BufferVisibleTarget   s:palette.yellow  s:palette.bg3   'bold'

-- BufferInactive        s:palette.grey1   s:palette.bg3
-- BufferInactiveIndex   s:palette.grey1   s:palette.bg3
-- BufferInactiveMod     s:palette.grey1   s:palette.bg3
-- BufferInactiveSign    s:palette.grey0   s:palette.bg3
-- BufferInactiveTarget  s:palette.yellow  s:palette.bg3   'bold'

-- BufferTabpages        s:palette.bg0     s:palette.grey2 'bold'
-- BufferTabpageFill     s:palette.bg0     s:palette.bg0

local colors = {
   gmd = { -- gruvbox-material dark
      background  = '#1E1E1E',
      active_buf  = { fg = '#e2cca9', bg = '#5a524c' },
      visible_buf = { fg = '#e2cca9', bg = '#45403d' },
      hidden_buf  = { fg = '#928374', bg = '#45403d' },
   }
}

local highlights = { -- {{{
   ['gruvbox-material'] = {
      fill = { -- background of the bufferline
         guibg = colors.gmd.background,
      },
      -- tab = { -- non-active tabpage (in vim sence)
      --    guifg = '#e9b143',
      --    guibg = '#e9b143',
      -- },
      -- tab_selected = { -- non-active tabpage (in vim sence)
      --    guifg = '#e9b143',
      --    guibg = '#e9b143',
      -- }

      -- -- pick_selected = {
      -- -- pick_visible = {
      -- -- pick = {
      --    -- guifg = '#e9b143',
      --    guibg = '#e9b143',
      -- }

      -- Active buffer
      buffer_selected = {
         guifg = colors.gmd.active_buf.fg,
         guibg = colors.gmd.active_buf.bg,
      },
      separator_selected = {
         guifg = colors.gmd.background,
         guibg = colors.gmd.active_buf.bg,
      },
      modified_selected = {
         guifg = '#80aa9e',
         guibg = colors.gmd.active_buf.bg,
      },
      close_button_selected = {
         -- guifg = '',
         guibg = colors.gmd.active_buf.bg,
      },

      -- Hidden buffers
      separator = {
         guifg = colors.gmd.background,
         guibg = colors.gmd.hidden_buf.bg,
      },
      background = { -- the body of non-active buffer
         guifg = colors.gmd.hidden_buf.fg,
         guibg = colors.gmd.hidden_buf.bg,
      },
      modified = {
         guifg = colors.gmd.hidden_buf.fg,
         guibg = colors.gmd.hidden_buf.bg,
      },
      close_button = {
         guibg = colors.gmd.hidden_buf.bg,
      },

      -- Buffer visible in non-active window
      buffer_visible = { -- visible in non-active window
         guifg = colors.gmd.visible_buf.fg,
         guibg = colors.gmd.visible_buf.bg,
      },
      separator_visible = {
         guifg = colors.gmd.background,
         guibg = colors.gmd.visible_buf.bg,
      },
      indicator_selected = {
         -- guifg = '#80aa9e',
         -- guibg = colors.gmd.visible_buf.bg,
      },
      modified_visible = {
         guifg = '#80aa9e',
         guibg = colors.gmd.visible_buf.bg,
      },
      close_button_visible = {
         -- guifg = '',
         guibg = colors.gmd.visible_buf.bg,
      },

   },
} --}}}

require("bufferline").setup {
   -- highlights = highlights[vim.g.colors_name] and highlights[vim.g.colors_name] or {},
   highlights = highlights['gruvbox-material'],
   options = {
      separator_style = 'slant', -- 'thick' | 'thin' | { 'any', 'any' },

      -- sort_by = 'extension' | 'relative_directory' | 'directory' | function(buffer_a, buffer_b)
      --   -- add custom logic
      --   return buffer_a.modified > buffer_b.modified
      -- end
      sort_by = 'relative_directory',
   },
}
