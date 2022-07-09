if not pcall(require, 'heirline') then return end

local prequire = require('util').prequire

local os_sep = package.config:sub(1,1)
local conditions = require('heirline.conditions')
local heirline = require("heirline.utils")
local devicons = prequire('nvim-web-devicons')
-- local nvim_gps_available, nvim_gps = pcall(require, 'nvim-gps')
-- local dap_available, dap = pcall(require, 'dap')
local nvim_gps = prequire('nvim-gps')
local dap = prequire('dap')
local util = require('anuvyklack.heirline.util')
local icons = util.icons
local mode = util.mode
local hydra = prequire('hydra.statusline')

local theme, theme_available = prequire('anuvyklack/heirline/themes/'..vim.g.colors_name)
-- local theme_available, theme = true, require('anuvyklack/heirline/themes/gruvbox-material')
if not theme_available then return end
local hl = theme.highlight
local colors = theme.colors
local mode_colors = hl.ModeColors
local lsp_colors = theme.lsp_colors

vim.o.showmode = false

-- Flexible components priorities
local priority = {
   CurrentPath = 60,
   Git = 40,
   WorkDir = 25,
   Lsp = 10,
}

local Align = { provider = '%=' }
local Space = setmetatable({ provider = ' ' }, {
   __call = function(_, n)
      return { provider = string.rep(' ', n) }
   end
})
local null  = { provider = '' }

local LeftCap = {
   provider = '▌',
   -- provider = '',
   hl = mode_colors.normal
}

local ReadOnly = {
   condition = function()
      return not vim.bo.modifiable or vim.bo.readonly
   end,
   provider = icons.padlock,
   hl = function()
      if conditions.is_active() then
         return hl.ReadOnly
      else
         return hl.NonActive
      end
   end
}

local NormalModeIndicator = {
   Space,
   {
      init = heirline.pick_child_on_condition,
      ReadOnly,
      {
         provider = icons.circle,
         hl = function()
            if vim.bo.modified then
               return { fg = mode_colors.insert.bg }
            elseif conditions.is_active() then
               return mode_colors.normal
            else
               return hl.VimMode.non_active
            end
         end
      }
   },
   Space
}

local HydraActive = {
   condition = hydra.is_active,
   heirline.surround(
      { icons.powerline.left_rounded, icons.powerline.right_rounded },
      function() -- color
         return theme.hydra[hydra.get_color()]
      end,
      {
         {
            init = heirline.pick_child_on_condition,
            ReadOnly,
            { provider = icons.circle }
         },
         Space,
         {
            provider = function()
               return hydra.get_name() or 'HYDRA'
            end,
         },
         hl = { fg = hl.StatusLine.active.bg, force = true },
      }
   )
}

-- local HydraActive = {
--    condition = hydra.is_active,
--    heirline.surround(
--       { icons.powerline.left_rounded, icons.powerline.right_rounded },
--       function() -- color
--          -- return mode_colors.normal.fg
--          return colors.grey0
--       end,
--       {
--          {
--             provider = icons.circle,
--             hl = function()
--                return { fg = theme.hydra[hydra.get_color()] }
--             end,
--          },
--          Space,
--          {
--             provider = function()
--                return hydra.get_name() or 'HYDRA'
--             end,
--             hl = { fg = hl.StatusLine.active.bg },
--          },
--       }
--    )
-- }

-- local HydraActive = {
--    condition = hydra.is_active,
--    Space,
--    {
--       provider = icons.circle,
--       hl = function()
--          return { fg = theme.hydra[hydra.get_color()] }
--       end
--    },
--    Space,
--    {
--       provider = function()
--          return hydra.get_name() or 'HYDRA'
--       end,
--       -- hl = { fg = hl.StatusLine.active.bg },
--    },
-- }

local HydraHint = {
   condition = function()
      return hydra.get_hint() and hydra.get_name() ~= 'Barbar'
   end,
   provider = hydra.get_hint,
}

local VimModeNormal = {
   condition = function(self)
      return self.mode == 'normal' or
             not conditions.is_active()
   end,
   {
      init = heirline.pick_child_on_condition,
      HydraActive,
      NormalModeIndicator
   }
}

local VimModeActive = {
   condition = function(self)
      return conditions.is_active() and self.mode ~= 'normal'
   end,
   hl = { bg = hl.StatusLine.bg },
   heirline.surround(
      { icons.powerline.left_rounded, icons.powerline.right_rounded },
      function(self) -- color
         return mode_colors[self.mode].bg
      end,
      {
         {
            init = heirline.pick_child_on_condition,
            ReadOnly,
            { provider = icons.circle }
         },
         Space,
         {
            provider = function(self)
               return util.mode_lable[self.mode]
            end,
         },
         hl = function(self)
            return mode_colors[self.mode]
         end
      }
   )
}

local VimMode = {
   init = function(self)
      self.mode = mode[vim.fn.mode(1)] -- :h mode()
   end,
   VimModeActive, VimModeNormal, Space
}

local FileIcon = {
   condition = function()
      return not ReadOnly.condition()
   end,
   init = function(self)
      local filename = self.filename
      local extension = vim.fn.fnamemodify(filename, ":e")
      self.icon, self.icon_color = devicons.get_icon_color(
         filename, extension, { default = true })
   end,
   provider = function(self)
      if self.icon then return self.icon .. ' ' end
   end,
   hl = function(self)
      return { fg = self.icon_color }
   end
}

local WorkDir = {
   condition = function(self) return self.pwd end,
   hl = hl.WorkDir,
   heirline.make_flexible_component(priority.WorkDir, {
      provider = function(self) return self.pwd end,
   },{
      provider = function(self)
         return vim.fn.pathshorten(self.pwd)
      end,
   }, null)
}

local CurrentPath = {
   condition = function(self) return self.current_path end,
   heirline.make_flexible_component(priority.CurrentPath, {
      provider = function(self) return self.current_path end,
   },{
      provider = function(self)
         return vim.fn.pathshorten(self.current_path, 2)
      end,
   },{
      provider = ''
   }),
   hl = hl.CurrentPath,
}

local FileName = {
   provider = function(self) return self.filename end,
   hl = hl.FileName
}

local GPS = {
   condition = function()
      -- return nvim_gps_available and nvim_gps.is_available()
      return nvim_gps.is_available()
   end,
   provider = function()
      local location = nvim_gps.get_location()
      if location ~= '' then
         return '> ' .. location
      end
   end,
   hl = hl.GPS
}

local FileProperties = {
   condition = function(self)
      self.filetype = vim.bo.filetype

      local encoding = (vim.bo.fileencoding ~= '' and vim.bo.fileencoding)
                       or vim.o.encoding
      self.encoding = (encoding ~= 'utf-8') and encoding or nil
      -- self.encoding = encoding

      local fileformat = vim.bo.fileformat

      -- if fileformat == 'dos' then
      --    fileformat = ' '
      -- elseif fileformat == 'mac' then
      --    fileformat = ' '
      -- else  -- unix'
      --    fileformat = ' '
      --    -- fileformat = nil
      -- end

      if fileformat == 'dos' then
         fileformat = 'CRLF'
      elseif fileformat == 'mac' then
         fileformat = 'CR'
      else  -- unix'
         -- fileformat = 'LF'
         fileformat = nil
      end

      self.fileformat = fileformat

      return self.fileformat or self.encoding
   end,
   provider = function(self)
      local sep
      if self.fileformat and self.encoding then sep = ' ' end
      return table.concat{ ' ', self.fileformat or '', sep or '', self.encoding or '', ' ' }
   end,
   hl = hl.FileProperties,
}

local FileNameBlock = {
   {
      init = heirline.pick_child_on_condition,
      HydraHint,
      {
         condition = conditions.is_active,
         FileIcon, WorkDir, CurrentPath, FileName
      },{
         FileIcon, FileName,
         hl = hl.NonActive
      },
   },
   -- This means that the statusline is cut here when there's not enough space.
   { provider = '%<' }
}

local DapMessages = {
   -- display the dap messages only on the debugged file
   condition = function()
      -- local session = dap_available and dap.session()
      local session = dap.session()
      if session then
         local filename = vim.api.nvim_buf_get_name(0)
         if session.config then
            local progname = session.config.program
            return filename == progname
         end
      end
      return false
   end,
   provider = function()
      return ' ' .. dap.status() .. ' '
   end,
   hl = hl.DapMessages
}

local Diagnostics = {
   condition = conditions.has_diagnostics,
   static = {
      -- error_icon = '󰂭 ',
      error_icon = vim.fn.sign_getdefined('DiagnosticSignError')[1].text,
      warn_icon  = vim.fn.sign_getdefined('DiagnosticSignWarn')[1].text,
      info_icon  = vim.fn.sign_getdefined('DiagnosticSignInfo')[1].text,
      hint_icon  = vim.fn.sign_getdefined('DiagnosticSignHint')[1].text,
   },
   init = function(self)
      self.errors   = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
      self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
      self.hints    = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
      self.info     = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
   end,
   {
      provider = function(self)
         -- 0 is just another output, we can decide to print it or not!
         if self.errors > 0 then
            return table.concat{ self.error_icon, self.errors, ' ' }
         end
      end,
      hl = hl.Diagnostic.error
   },
   {
      provider = function(self)
         if self.warnings > 0 then
            return table.concat{ self.warn_icon, self.warnings, ' ' }
         end
      end,
      hl = hl.Diagnostic.warn
   },
   {
      provider = function(self)
         if self.info > 0 then
            return table.concat{ self.info_icon, self.info, ' ' }
         end
      end,
      hl = hl.Diagnostic.info
   },
   {
      provider = function(self)
         if self.hints > 0 then
            return table.concat{ self.hint_icon, self.hints, ' ' }
         end
      end,
      hl = hl.Diagnostic.hint
   },
   Space(2)
}

local GitBranch = {
   condition = conditions.is_git_repo,
   init = function(self)
      self.git_status = vim.b.gitsigns_status_dict
   end,
   hl = hl.Git.branch,
   provider = function(self)
      return table.concat{ " ", self.git_status.head, ' ' }
   end,
}

local GitChanges = {
   condition = function(self)
      if conditions.is_git_repo() then
         self.git_status = vim.b.gitsigns_status_dict
         local has_changes = self.git_status.added   ~= 0 or
                             self.git_status.removed ~= 0 or
                             self.git_status.changed ~= 0
         return has_changes
      end
   end,
   {
      provider = function(self)
         local count = self.git_status.added or 0
         return count > 0 and table.concat{'+', count, ' '}
         -- return count > 0 and table.concat{'● ', count, ' '}
      end,
      hl = hl.Git.added
   },
   {
      provider = function(self)
         local count = self.git_status.changed or 0
         return count > 0 and table.concat{'~', count, ' '}
         -- return count > 0 and table.concat{'● ', count, ' '}
      end,
      hl = hl.Git.changed
   },
   {
      provider = function(self)
         local count = self.git_status.removed or 0
         return count > 0 and table.concat{'-', count, ' '}
         -- return count > 0 and table.concat{'● ', count, ' '}
      end,
      hl = hl.Git.removed
   },
   Space
}

local Git = heirline.make_flexible_component(priority.Git,
   { GitBranch, GitChanges },
   { GitBranch }
)

local LspIndicator = {
   provider = icons.circle_small .. ' ',
   hl = hl.LspIndicator
}

local LspServer = {
   Space,
   {
      provider = function(self)
         local names = self.lsp_names
         if #names == 1 then
            names = names[1]
         else
            names = table.concat(vim.tbl_flatten({ '[', names, ']' }), ' ')
         end
         return names
      end,
   },
   Space(2),
   hl = hl.LspServer
}

local Lsp = {
   condition = conditions.lsp_attached,
   init = function(self)
      local names = {}
      for _, server in pairs(vim.lsp.buf_get_clients(0)) do
          table.insert(names, server.name)
      end
      self.lsp_names = names
   end,
   heirline.make_flexible_component(priority.Lsp, LspServer, LspIndicator),
   hl = function(self)
      local color
      for _, name in ipairs(self.lsp_names) do
         if lsp_colors[name] then
            color = lsp_colors[name]
            break
         end
      end
      if color then
         return { fg = color, bold = true, force = true }
      else
         return hl.LspServer
      end
   end
}

local SearchResults = {
   condition = function(self)
      local lines = vim.api.nvim_buf_line_count(0)
      if lines > 50000 then return end

      local query = vim.fn.getreg("/")
      if query == "" then return end

      if query:find("@") then return end

      local search_count = vim.fn.searchcount({ recompute = 1, maxcount = -1 })
      local active = false
      if vim.v.hlsearch and vim.v.hlsearch == 1 and search_count.total > 0 then
         active = true
      end
      if not active then return end

      query = query:gsub([[^\V]], "")
      query = query:gsub([[\<]], ""):gsub([[\>]], "")

      self.query = query
      self.count = search_count
      return true
   end,
   {
      provider = function(self)
         return table.concat {
            -- ' ', self.query, ' ', self.count.current, '/', self.count.total, ' '
            ' ', self.count.current, '/', self.count.total, ' '
         }
      end,
      hl = hl.SearchResults
   },
   Space
}

local Ruler = {
   -- %-2 : make item takes at least 2 cells and be left justified
   -- %l  : current line number
   -- %L  : number of lines in the buffer
   -- %c  : column number
   -- provider = ' %7(%l:%3L%)  %-2c ',
   provider = ' %7(%l:%L%)  %-2c ',
   hl = { bold = true }
}

local ScrollPercentage = {
   condition = function() return conditions.width_percent_below(4, 0.035) end,
   -- %P  : percentage through file of displayed window
   provider = ' %3(%P%)',
   hl = function()
      if conditions.is_active() then
         return hl.StatusLine.active
      else
         return hl.StatusLine.non_active
      end
   end
}

local ScrollBar = {
   static = {
      sbar = { '🭶', '🭷', '🭸', '🭹', '🭺', '🭻' }
   },
   provider = function(self)
      local curr_line = vim.api.nvim_win_get_cursor(0)[1]
      local lines = vim.api.nvim_buf_line_count(0)
      local i = math.floor((curr_line - 1) / lines * #self.sbar) + 1
      return string.rep(self.sbar[i], 2)
   end,
   hl = function()
      if conditions.is_active() then
         return hl.ScrollBar.active
      else
         return hl.ScrollBar.non_active
      end
   end
}

local ActiveStatusline = {
   LeftCap, VimMode,
   SearchResults,
   FileNameBlock,
   Space(4),
   -- GPS,
   Align,
   DapMessages,
   Diagnostics, Git, Lsp,
   FileProperties,
   Ruler, ScrollBar, ScrollPercentage
}

local InactiveStatusline = {
   condition = function() return not conditions.is_active() end,
   Space, VimMode,
   FileNameBlock, Align, ScrollBar, ScrollPercentage
}

local HelpBufferStatusline = {
   condition = function()
      return vim.bo.filetype == "help"
   end,
   Space, VimMode,
   {
      provider = function()
         local filename = vim.api.nvim_buf_get_name(0)
         return vim.fn.fnamemodify(filename, ":t")
      end,
      hl = function ()
         if conditions.is_active() then
            return hl.FileName.active
         else
            return hl.FileName.non_active
         end
      end
   },
   Align,
   ScrollBar,ScrollPercentage
}

-- local SpecialStatusline = {
--    condition = function()
--       return conditions.buffer_matches({
--          buftype = { "nofile", "prompt", "help", "quickfix" },
--          filetype = { "^git.*", "fugitive" },
--       })
--    end,
--    -- FileType, Space, HelpFileName, Align
-- }

-- local TerminalStatusline = {
--    condition = function()
--       return conditions.buffer_matches({ buftype = { "terminal" } })
--    end,
--    hl = { bg = colors.dark_red },
--    -- Quickly add a condition to the ViMode to only show it when buffer is active!
--    { condition = conditions.is_active, ViMode, Space }, FileType, Space, TerminalName, Align,
-- }

local StatusLines = {
   init = function(self)
      local pwd = vim.fn.getcwd(0) -- Present working directory.
      local current_path = vim.api.nvim_buf_get_name(0)
      local filename

      if current_path == "" then
         pwd = vim.fn.fnamemodify(pwd, ':~')
         current_path = nil
         filename = ' [No Name]'
      elseif current_path:find(pwd, 1, true) then
         filename = vim.fn.fnamemodify(current_path, ':t')
         current_path = vim.fn.fnamemodify(current_path, ':~:.:h')
         pwd = vim.fn.fnamemodify(pwd, ':~') .. os_sep
         if current_path == '.' then
            current_path = nil
         else
            current_path = current_path .. os_sep
         end
      else
         pwd = nil
         filename = vim.fn.fnamemodify(current_path, ':t')
         current_path = vim.fn.fnamemodify(current_path, ':~:.:h') .. os_sep
      end

      self.pwd = pwd
      self.current_path = current_path -- The opened file path relevant to pwd.
      self.filename = filename

      heirline.pick_child_on_condition(self)
   end,
   hl = function()
      if conditions.is_active() then
         return hl.StatusLine.active
      else
         return hl.StatusLine.non_active
      end
   end,
   HelpBufferStatusline, InactiveStatusline, ActiveStatusline
}

require('heirline').setup(StatusLines)

-- vim: fml=2
