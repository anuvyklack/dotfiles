local api, fn = vim.api, vim.fn
local strdisplaywidth = vim.fn.strdisplaywidth

local path_limit = 30 -- Maximum number of chars in the displayed file path.
local valid_token = '{'
local non_valid_token = '}'

function _G.quickfixtextfunc(info)
   local items
   if info.quickfix == 1 then
      items = fn.getqflist({ id = info.id, items = 0 }).items
   else
      items = fn.getloclist(info.winid, { id = info.id, items = 0 }).items
   end
   local lines = {}

   ---@type boolean
   local align = true
   ---@type false | string
   local common_path
   ---@type table<integer, { path:string, buf_pos:string, len:integer }>
   local locations = {}
   for i = info.start_idx, info.end_idx do
      local item = items[i]
      if item.valid == 1 then
         local bufnr = item.bufnr
         -- Quickfix list entries, returned by getqflist funciton, with
         -- a non-existing buffer number are returned with "bufnr" set to zero,
         -- but for most functions zero is the current buffer, so we need to
         -- need to explicitly check for zero.
         if bufnr > 0 then
            -- local path = fn.fnamemodify(api.nvim_buf_get_name(bufnr), ':~:.')
            local path = api.nvim_buf_get_name(bufnr)
            if common_path == nil then
               common_path = path
            elseif common_path and common_path ~= path then
               common_path = false
            end
            if path == '' then path = '[No Name]' end
            local lnum = item.lnum > 99999 and -1 or item.lnum
            local col = item.col > 999 and -1 or item.col
            local buf_position = table.concat({ ':', lnum, ':', col })
            locations[i] = { path = path, buf_pos = buf_position }
         end
      else
         align = false
      end
   end

   if common_path then
      common_path = fn.fnamemodify(common_path, ':t')
   end

   local align_len = 0
   local cplen = common_path and strdisplaywidth(common_path)
   for _, loc in pairs(locations) do
      loc.path = common_path or fn.fnamemodify(loc.path, ':~:.')
      if align then
         local len = (cplen or strdisplaywidth(loc.path)) + #loc.buf_pos
         if align_len < len then align_len = len end
         loc.len = len
      end
   end

   for i = info.start_idx, info.end_idx do
      local item = items[i]
      local line
      if item.valid == 1 then
         local l = locations[i]
         local location = l.path .. l.buf_pos
         if align then
            location = location .. string.rep(' ', align_len - l.len)
         end
         local error_type = (item.type ~= '') and item.type..' ' or ''
         line = table.concat({ valid_token, location, ' ', error_type, item.text })
      else
         line = non_valid_token .. item.text
      end
      table.insert(lines, line)
   end

   return lines
end

vim.o.quickfixtextfunc = '{info -> v:lua._G.quickfixtextfunc(info)}'
