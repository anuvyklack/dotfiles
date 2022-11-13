local api, fn = vim.api, vim.fn
local termguicolors = vim.o.termguicolors
local comment_hl = api.nvim_get_hl_by_name('Comment', termguicolors)
local empty_str_pattern = '^%s+$'
local M = {}

---@param chunk UfoExtmarkVirtTextChunk
function M.is_comment(chunk)
   local hl_id = chunk[2]
   if type(hl_id) == 'number' then
      local hl = api.nvim_get_hl_by_id(hl_id, termguicolors)
      return vim.deep_equal(hl, comment_hl)
   else
      return false
   end
end

---@param chunk UfoExtmarkVirtTextChunk
function M.is_empty(chunk)
   if not chunk then return false end
   local text = chunk[1]
   if text:find(empty_str_pattern) then
      return true
   else
      return false
   end
end

function M.make_fold_marker_pattern()
   local fold_marker = vim.opt.foldmarker:get()[1]
   return table.concat{ '%s?', vim.pesc(fold_marker), '%d*' }
end

---Return a list with a pattern per every comment token, that will match only
---a string that contains only a comment token and any number of spaces before or
---after.
---@return string[]
function M.make_only_comment_token_patterns()
   ---@type string[]
   local comment_tokens = fn.split(vim.bo.commentstring, '%s') -- or {''}

   -- Trim redundant spaces from the beggining and the end if any.
   if not vim.tbl_isempty(comment_tokens) then
      for i = 1, #comment_tokens do
         comment_tokens[i] = vim.trim(comment_tokens[i])
      end
   end

   local patterns = {}
   for i, token in ipairs(comment_tokens) do
      patterns[i] = table.concat{ '^%s*', vim.pesc(token), '%s*$' }
   end

   return patterns
end

return M
