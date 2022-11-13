local util = require('anuvyklack.ufo.util')
local strdisplaywidth = vim.fn.strdisplaywidth
local is_comment = util.is_comment
local is_empty = util.is_empty

local function make_ts_handler()
   -- local folded_sign = '⋯'
   local folded_sign = '…'

   local only_comment_token_patterns = util.make_only_comment_token_patterns()
   local fold_marker_patten = util.make_fold_marker_pattern()
   local spaces_at_beginning_pattern = '^%s+'

   ---@param virt_text UfoExtmarkVirtTextChunk[]
   ---@param fold_start integer
   ---@param fold_end integer
   ---@param available_width integer
   ---@param truncate function
   ---@param context UfoFoldVirtTextHandlerContext
   ---@return UfoExtmarkVirtTextChunk[]
   local function handler(virt_text, fold_start, fold_end, available_width, truncate, context)
      -- print(vim.api.nvim_get_hl_id_by_name('luaTSComment'))
      -- P(virt_text)
      -- P(context.get_fold_virt_text(fold_end))

      local last_line = context.get_fold_virt_text(fold_end)
      local orig_text_width = context.text:len()

      local new_virt_text = {} ---@type UfoExtmarkVirtTextChunk[]
      local nvrt_width = 0 -- new_virt_text width

      ---@param chunk UfoExtmarkVirtTextChunk
      ---@return boolean continue Can we continue add chunks?
      local function add_chunk(chunk)
         -- Remove fold marker from comment
         if is_comment(chunk) then
            chunk[1] = chunk[1]:gsub(fold_marker_patten, '')
            for _, pat in ipairs(only_comment_token_patterns) do
               if chunk[1]:find(pat) then
                  -- Will continue since we don't add this chunk.
                  return true
               end
            end
         end

         local continue = true
         local chunk_text = chunk[1]
         local chunk_width = strdisplaywidth(chunk_text)
         if available_width > nvrt_width + chunk_width then
            table.insert(new_virt_text, chunk)
            continue = true
         else
            chunk_text = truncate(chunk_text, available_width - nvrt_width)
            chunk_width = strdisplaywidth(chunk_text)
            local hl_group = chunk[2]
            table.insert(new_virt_text, { chunk_text, hl_group })
            continue = false

            -- -- str width returned from truncate() may less than 2nd argument, need padding
            -- if cur_width + chunk_width < available_width then
            --    suffix = suffix..(' '):rep(available_width - cur_width - chunk_width)
            -- end
         end
         nvrt_width = nvrt_width + chunk_width
         return continue
      end

      ---@return boolean continue Can we continue add chunks?
      local function add_end_virt_text()
         if is_empty(last_line[1]) then
            table.remove(last_line, 1)
         end

         local last_chunk = last_line[#last_line]
         if last_chunk and is_comment(last_chunk) then
            table.remove(last_line)
         end
         last_chunk = last_line[#last_line]
         if last_chunk and is_empty(last_chunk) then
            table.remove(last_line)
         end

         -- for i, chunk in ipairs(last_line) do
         --    if not (i == 1 and is_empty(chunk))
         --       and not (i == #last_line and is_comment(chunk))
         --    then
         --       continue = add_chunk(chunk)
         --       if not continue then break end
         --    end
         -- end

         local continue = true
         for _, chunk in ipairs(last_line) do
            continue = add_chunk(chunk)
            if not continue then break end
         end
         return continue
      end

      local continue = true
      if #virt_text == 2 and is_empty(virt_text[1])
         and is_comment(virt_text[2])
      then
         for _, chunk in ipairs(virt_text) do
            continue = add_chunk(chunk)
            if not continue then break end
         end
      else
         for i, chunk in ipairs(virt_text) do
            if i ~= #virt_text then
               continue = add_chunk(chunk)
            elseif #virt_text == 1 or not is_comment(chunk) then
               continue = add_chunk(chunk) and
                          add_chunk({' '..folded_sign..' ', 'Comment'}) and
                          add_end_virt_text()
            else
               continue = add_chunk({folded_sign..' ', 'Comment'}) and
                          add_end_virt_text() and
                          add_chunk({' ', 'UfoFoldedFg'}) and
                          add_chunk(chunk)
            end
            if not continue then break end
         end
      end

      if nvrt_width < orig_text_width then
         add_chunk({string.rep(' ', orig_text_width - nvrt_width), 'UfoFoldedFg'})
      end

      -- if continue then
      --    local suffix = ('  %d '):format(fold_end - fold_start)
      --    continue =
      --       add_chunk({string.rep(' ', available_width - cur_width - 10), 'UfoFoldedFg'})
      --       and add_chunk({ suffix, 'MoreMsg' })
      -- end

      return new_virt_text
   end

   return handler
end

return make_ts_handler

