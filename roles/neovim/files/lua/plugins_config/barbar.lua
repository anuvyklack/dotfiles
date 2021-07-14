vim.g.bufferline = {

  -- Enable/disable auto-hiding the tab bar when there is a single buffer.
  auto_hide = false,

  -- Icons on the bufferline.
  -- ------------------------
  -- icon_separator_active = '▎',
  -- icon_separator_inactive = '▎',

  -- icon_separator_active = '▊',
  -- icon_separator_inactive = '▍',

  icon_separator_active = ' ',
  icon_separator_inactive = '',

  icon_close_tab = '',
  icon_close_tab_modified = '●',


  -- Sets the maximum padding width with which to surround each tab.
  maximum_padding = 4,
}

require('keybindings').barbar()
