local bo = vim.bo
local v = vim.v
local wo = vim.wo
local opt = vim.opt
local fn = vim.fn

local fill_char = '•'
local default_config = {
   fill_char = fill_char,
   remove_fold_markers = true;
   sections = {
      left = {
         'content',
      },
      right = {
         'number_of_folded_lines',
         string.rep(fill_char, 2),
         -- ':',
         'percentage'
      }
   }
}


local service_sections = {
   number_of_folded_lines = function(config)
      local fold_size = v.foldend - v.foldstart + 1  -- The number of folded lines.
      return config.fill_char..' '..fold_size..' lines '
   end,
   percentage = function()
      local fold_size = v.foldend - v.foldstart + 1  -- The number of folded lines.
      return ' ' .. math.floor(100 * fold_size / vim.api.nvim_buf_line_count(0)) .. '% '
   end
}

---@return string content the content of the first nonblank line of the folding region
function service_sections.content(config)
   local line_num = v.foldstart
   local content = fn.getline(line_num)
   local indent_num = fn.indent(line_num)

   local comment_signs = vim.split(bo.commentstring, '%s')

   -- Remove all fold markers from string.
   if config.remove_fold_markers then
      for _, fdm in ipairs( opt.foldmarker:get() ) do
         content = content:gsub(fdm..'%d*', '')
      end

      -- Remove all comment signs from the end of the string.
      for i = #comment_signs, 1, -1 do  -- Iterate backward from the end of the list.
         content = content:gsub('%s*'..comment_signs[i]..'%s*$', '')
      end
   end

   -- If after removimg fold markers and comment signs we get blank line,
   -- take next nonblank.
   if content:match('^%s*$') then
      line_num = fn.nextnonblank(v.foldstart + 1)
      if line_num ~= 0 and line_num <= v.foldend then
         content = fn.getline(line_num)
         indent_num = fn.indent(line_num)
      end
   end

   if config.sections.left[1] == 'content' then
      if indent_num > 1 then
         -- Replace indentation with 'fill_char'-s.
         content = content:gsub('^%s+', string.rep(config.fill_char, indent_num - 1)..' ')
      end
   else
      content = content:gsub('^%s*', ' ')  -- Strip all indentation.
   end

   -- HACK: I tried to use:
   --    content:gsub('%s*$', ' ')
   -- but get strange bug: sometimes it insers 2 spaces at the end of the line,
   -- so this is workaround.
   content = content:gsub('%s*$', '')..' '

   -- Exchange all spaces between comment sign and text with 'fill_char'.
   -- For example: '//       Text' -> '// +++++ Text'
   local blank_substr = content:match( comment_signs[1]..'(%s+)' ) or ''
   if #blank_substr > 2 then
      content = content:gsub(
         comment_signs[1]..'(%s+)',
         comment_signs[1]..' '..string.rep(config.fill_char, #blank_substr - 2)..' ',
         1)
   end

   -- Replace all tabs with spaces with respect to %tabstop.
   content = content:gsub('\t', string.rep(' ', bo.tabstop))

   return content
end

local function unknown_section(_, custom_section)
   -- return custom_section
   if vim.is_callable(custom_section) then
      return custom_section()
   else
      return custom_section
   end
end

local sections = setmetatable(service_sections, { __index = unknown_section })

-- It skips first blank line or line that contains only comment sign and folder
-- mark.
function _G.custom_fold_text(config)
   config = vim.tbl_deep_extend("force", default_config, config or {})

   local r = { left = {}, right = {} }
   for _, lr in ipairs({'left', 'right'}) do
      for _, s in ipairs(config.sections[lr] or {}) do
         local sec = sections[s]
         if vim.is_callable(sec) then
            table.insert(r[lr], sec(config))
         else
            table.insert(r[lr], sec)
         end
      end
   end

   if config.sections.right and
      not vim.tbl_isempty(config.sections.right)
   then
      -- The width of the number, fold and sign columns.
      local num_col_width = math.min( fn.strlen(fn.line('$')), wo.numberwidth )
      local fold_col_width = wo.foldcolumn:match('%d+$') or 3
      local sign_col_width = wo.signcolumn:match('%d+$') * 2 or 6

      local visible_win_width =
         vim.api.nvim_win_get_width(0) - num_col_width - fold_col_width - sign_col_width

      local lnum = 0
      for _, str in ipairs( vim.tbl_flatten( vim.tbl_values(r) ) ) do
         -- lnum = lnum + #str
         lnum = lnum + fn.strdisplaywidth(str)
      end
      r.expansion_str = string.rep(config.fill_char, visible_win_width - lnum - 4)
   else
      r.expansion_str = ''
   end

   local result = ''
   for _, str in ipairs(r.left) do
      result = result .. str
   end
   result = result .. r.expansion_str
   for _, str in ipairs(r.right) do
      result = result .. str
   end

   return result
end


opt.fillchars:append('fold:•')
opt.foldtext = 'v:lua.custom_fold_text()'
-- opt.foldtext = 'v:lua.custom_fold_text("•")'
