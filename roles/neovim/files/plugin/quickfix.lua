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

   ---@type integer | boolean
   local align = 0
   for i = info.start_idx, info.end_idx do
      local item = items[i]
      if item.valid == 1 then
         local bufnr = item.bufnr
         if bufnr > 0 then
            local path = fn.fnamemodify(api.nvim_buf_get_name(bufnr), ':~:.')
            if path == '' then
               path = '[No Name]'
            end
            local lnum = item.lnum > 99999 and -1 or item.lnum
            local col = item.col > 999 and -1 or item.col
            local location = table.concat({ path, ':', lnum, ':', col })
            if align then
               local N = strdisplaywidth(location)
               if align < N then align = N end
            end
            item.location = location
         end
      else
         align = false
      end
   end

   for i = info.start_idx, info.end_idx do
      local item = items[i]
      local line
      if item.valid == 1 then
         local location = item.location
         if align then
            local N = strdisplaywidth(location)
            location = location .. string.rep(' ', align - N)
         end
         local error_type = (item.type ~= '') and item.type..' ' or ''
         line = table.concat({
            valid_token, location, ' ', error_type, item.text
         })
      else
         line = non_valid_token .. item.text
      end
      table.insert(lines, line)
   end

   return lines
end

vim.o.quickfixtextfunc = '{info -> v:lua._G.quickfixtextfunc(info)}'
