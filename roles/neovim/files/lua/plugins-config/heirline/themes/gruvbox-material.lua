local heirline = require("heirline.utils")
local M = {}

--                      black             #282828
-- fg0        #e2cca9   bg0               #32302f
-- fg1        #f0dec2   bg1               #3c3836
--                      bg2               #3c3836
-- grey0      #7c6f64   bg3               #504945
-- grey1      #928374   bg4               #504945
-- grey2      #a89984   bg5               #665c54
-- grey3      #b0a392   bg_current_word   #45403d
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

local colors
do
   local background = vim.opt.background:get()
   local configuration = vim.fn['gruvbox_material#get_configuration']()
   colors = vim.fn['gruvbox_material#get_palette'](background, configuration.palette)
   for key, color in pairs(colors) do
      colors[key] = color[1]
   end

   -- Do not delete! It is my addition.
   colors.fg1   = '#f0dec2'
   colors.grey3 = '#b0a392'
   colors.black = '#282828'
end

colors.onedark = {
   blue   = '#8ab7d8',
   green  = '#98c369',
   yellow = '#ffff70',
   orange = '#ea9d70',
   purple = '#c475c1',
   red    = '#971717'
}

local non_active = {
   fg = colors.grey2,
   bg = colors.bg_statusline2,
   bold = true,
   force = true
}

local hl = {
   NonActive = non_active,

   StatusLine = {
      active = {
         fg = heirline.get_highlight('Statusline').fg,
         bg = colors.bg_statusline2,
         bold = true
      },
      non_active = { -- StatuslineNC - none current
         fg = colors.grey2,
         bg = colors.bg_statusline2,
         bold = true,
      }
   },

   VimMode  = {
      non_active = { fg = colors.grey1 }
   },

   ReadOnly = { fg = colors.red },

   -- WorkDir = { fg = heirline.get_highlight('Comment').fg, bold = true },
   WorkDir = { fg = colors.grey3, bold = true },

   CurrentPath = { fg = heirline.get_highlight('Directory').fg, bold = true },
   -- CurrentPath = { fg = colors.blue, bold = true },

   FileName = {
      fg = heirline.get_highlight('Statusline').fg,
      bold = true
   },

   GPS = { fg = colors.grey2 },

   FileProperties = nil,

   DapMessages = { fg = heirline.get_highlight('Debug').fg },

   Git = {
      branch  = { fg = colors.purple, bold = true },
      added   = { fg = colors.green,  bold = true },
      changed = { fg = colors.yellow, bold = true },
      removed = { fg = colors.red,    bold = true },
   },

   LspIndicator = { fg = colors.blue },
   LspServer = { fg = colors.onedark.blue, bold = true },

   Diagnostic = {
      error = { fg = heirline.get_highlight('DiagnosticSignError').fg },
      warn  = { fg = heirline.get_highlight('DiagnosticSignWarn').fg },
      info  = { fg = heirline.get_highlight('DiagnosticSignInfo').fg },
      hint  = { fg = heirline.get_highlight('DiagnosticSignHint').fg }
   },

   ScrollBar = {
      active     = { bg = colors.grey0, fg = colors.fg1 },
      non_active = { bg = colors.bg5, fg = colors.grey2 }
   },

   SearchResults = { fg = colors.black, bg = colors.aqua }

}

local mode_colors = {
   normal       = colors.grey2,
   op           = colors.blue,
   insert       = colors.blue,
   visual       = colors.bg_yellow,
   visual_lines = colors.bg_yellow,
   visual_block = colors.bg_yellow,
   replace      = colors.red,
   v_replace    = colors.red,
   enter        = colors.aqua,
   more         = colors.aqua,
   select       = colors.purple,
   command      = colors.aqua,
   shell        = colors.orange,
   term         = colors.orange,
   none         = colors.red,
}

local ModeColors = setmetatable({
   normal = { fg = mode_colors.normal }
},{
   __index = function(_, mode)
      return {
         -- fg = colors.black,
         fg = hl.StatusLine.active.bg,
         bg = mode_colors[mode],
         bold = true
      }
   end
})

local lsp_colors = {
   sumneko_lua = '#5EBCF6',
   vimls = '#43BF6C'
}

M.colors = colors
M.highlight = hl
M.highlight.ModeColors = ModeColors
M.lsp_colors = lsp_colors

return M
