local actions = require 'telescope.actions'
local action_state = require 'telescope.actions.state'
local M = {}

function M.auto_multi_selection_open_qflist(prompt_bufnr)
   local picker = action_state.get_current_picker(prompt_bufnr)
   local num_selections = #picker:get_multi_selection()
   if num_selections > 1 then
      actions.send_selected_to_qflist(prompt_bufnr)
      actions.open_qflist()
   else
      actions.select_default(prompt_bufnr)
   end
end

function M.auto_multi_selection_open_loclist(prompt_bufnr)
   local picker = action_state.get_current_picker(prompt_bufnr)
   local num_selections = #picker:get_multi_selection()
   if num_selections > 1 then
      actions.send_selected_to_loclist(prompt_bufnr)
      actions.open_loclist()
   else
      actions.select_default(prompt_bufnr)
   end
end

-- https://github.com/nvim-telescope/telescope.nvim/issues/1048
local function multiopen(prompt_bufnr, open_cmd)
   local picker = action_state.get_current_picker(prompt_bufnr)
   local num_selections = #picker:get_multi_selection()
   if num_selections > 1 then
      local cwd = picker.cwd
      if cwd == nil then
         cwd = ""
      else
         cwd = string.format("%s/", cwd)
      end
      vim.cmd("bw!") -- wipe the prompt buffer
      for _, entry in ipairs(picker:get_multi_selection()) do
         vim.cmd(string.format("%s %s%s", open_cmd, cwd, entry.value))
      end
      vim.cmd('stopinsert')
   else
      if open_cmd == "vsplit" then
         actions.file_vsplit(prompt_bufnr)
      elseif open_cmd == "split" then
         actions.file_split(prompt_bufnr)
      elseif open_cmd == "tabe" then
         actions.file_tab(prompt_bufnr)
      else
         actions.select_default(prompt_bufnr)
      end
   end
end

function M.multi_selection_open_vsplit(prompt_bufnr)
   multiopen(prompt_bufnr, "vsplit")
end

function M.multi_selection_open_split(prompt_bufnr)
   multiopen(prompt_bufnr, "split")
end

function M.multi_selection_open_tab(prompt_bufnr)
   multiopen(prompt_bufnr, "tabe")
end

function M.multi_selection_open(prompt_bufnr)
   multiopen(prompt_bufnr, "edit")
end

return M

