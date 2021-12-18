local v = vim.v
local bo = vim.bo
local wo = vim.wo
local opt = vim.opt
local fn = vim.fn

function _G.custom_fold_text(fill_char)
   fill_char = fill_char or ' '

   local line_num = v.foldstart
   local result = fn.getline(line_num)
   local indent_num = fn.indent(line_num)

   -- Remove all fold markers from string.
   for _, fdm in ipairs( opt.foldmarker:get() ) do
      result = result:gsub(fdm..'%d*', '')
   end

   -- Remove all comment signs from the end of the string.
   local comment_signs = fn.split(bo.commentstring, '%s')
   for i = #comment_signs, 1, -1 do  -- Iterate backward from the end of the list.
      result = result:gsub('%s*'..comment_signs[i]..'%s*$', '')
   end

   -- If after removimg fold markers and comment signs we get blank line,
   -- take next nonblank.
   if result:match('^%s*$') ~= nil then
      line_num = fn.nextnonblank(v.foldstart + 1)
      if line_num ~= 0 and line_num <= v.foldend then
         result = fn.getline(line_num)
         indent_num = fn.indent(line_num)
      end
   end

   if indent_num > 1 then
      result = result:gsub('^%s+', string.rep(fill_char, indent_num - 1)..' ')
   end

   -- I tried to use:
   --    result:gsub('%s*$', ' ')
   -- but get strange bug: sometimes it insers 2 spaces at the end of the line.
   result = result:gsub('%s*$', '')..' '

   -- Exchange all spaces between comment sign and text with 'fill_char'.
   -- For example: '//       Text' -> '// +++++ Text'
   local blank_substr = result:match( comment_signs[1]..'(%s+)' ) or ''
   if #blank_substr > 2 then
      result = result:gsub(
         comment_signs[1]..'(%s+)',
         comment_signs[1]..' '..string.rep(fill_char, #blank_substr - 2)..' ',
         1)
   end

   -- Replace all tabs with spaces with respect to %tabstop.
   result = result:gsub('\t', string.rep(' ', bo.tabstop))

   local fold_size_str = fill_char..' '..(v.foldend - v.foldstart + 1)..' lines '
   -- result = result .. fold_size_str

   -- The width of the number, fold and sign columns.
   local num_col_width = math.min( fn.strlen(fn.line('$')), wo.numberwidth )
   local fold_col_width = wo.foldcolumn:match('%d+$') or 3
   local sign_col_width = wo.signcolumn:match('%d+$') * 2 or 6

   local visible_win_width = (vim.api.nvim_win_get_width(0) - num_col_width
                              - fold_col_width - sign_col_width)

   local expansion_str = string.rep(fill_char,
      visible_win_width - fn.strdisplaywidth(result .. fold_size_str))

   return result .. expansion_str .. fold_size_str
end

opt.fillchars:append('fold:•')
opt.foldtext = 'v:lua.custom_fold_text("•")'
