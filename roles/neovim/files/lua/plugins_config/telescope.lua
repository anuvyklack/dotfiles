local available, telescope = pcall(require, "telescope")
if not available then return end

local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local custom_actions = {}

function custom_actions.auto_multi_selection_open_qflist(prompt_bufnr)
   local picker = action_state.get_current_picker(prompt_bufnr)
   local num_selections = #picker:get_multi_selection()
   if num_selections > 1 then
      actions.send_selected_to_qflist(prompt_bufnr)
      actions.open_qflist()
   else
      actions.select_default(prompt_bufnr)
   end
end

function custom_actions.auto_multi_selection_open_loclist(prompt_bufnr)
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
function custom_actions._multiopen(prompt_bufnr, open_cmd)
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
function custom_actions.multi_selection_open_vsplit(prompt_bufnr)
    custom_actions._multiopen(prompt_bufnr, "vsplit")
end
function custom_actions.multi_selection_open_split(prompt_bufnr)
    custom_actions._multiopen(prompt_bufnr, "split")
end
function custom_actions.multi_selection_open_tab(prompt_bufnr)
    custom_actions._multiopen(prompt_bufnr, "tabe")
end
function custom_actions.multi_selection_open(prompt_bufnr)
    custom_actions._multiopen(prompt_bufnr, "edit")
end

--------------------------- Telescope settngs ----------------------------------

telescope.setup {
   defaults = {
      prompt_prefix = " ",
      selection_caret = " ", --  卑 喝   
      dynamic_preview_title = true,
      sorting_strategy = 'ascending',
      scroll_strategy = 'limit',  -- or 'cycle'
      layout_strategy = 'flex', -- 'flex', 'horizontal', 'vertical',
      layout_config = {
         horizontal = {
            mirror = false,
            prompt_position = 'top',
            preview_width = 82,
         },
         vertical = {
            -- mirror = true,
            prompt_position = 'top',
            preview_height = 0.5,
         },
      },
      mappings = {
         -- ["<esc>"] = require('telescope.actions').close,
         -- ["<C-n>"] = false, -- disable keymap
         i = {  -- insert mode
            -- ["<C-k>"] = require('telescope.actions').move_selection_better,
            ["<C-k>"] = 'move_selection_previous',
            ["<C-j>"] = 'move_selection_next',

         -- ["<esc>"] = actions.close,
         -- ["<C-j>"] = actions.move_selection_next,
         -- ["<C-k>"] = actions.move_selection_previous,
         -- ["<tab>"] = actions.toggle_selection + actions.move_selection_next,
         -- ["<s-tab>"] = actions.toggle_selection + actions.move_selection_previous,
         -- ["<cr>"] = custom_actions.multi_selection_open,
         -- ["<c-v>"] = custom_actions.multi_selection_open_vsplit,
         -- ["<c-s>"] = custom_actions.multi_selection_open_split,
         -- ["<c-t>"] = custom_actions.multi_selection_open_tab,
         },
         n = {  -- normal mode
            ["<Space>"] = 'toggle_selection',

         -- ["<esc>"] = actions.close,
         -- ["<tab>"] = actions.toggle_selection + actions.move_selection_next,
         -- ["<s-tab>"] = actions.toggle_selection + actions.move_selection_previous,
         -- ["<cr>"] = custom_actions.multi_selection_open,
         -- ["<c-v>"] = custom_actions.multi_selection_open_vsplit,
         -- ["<c-s>"] = custom_actions.multi_selection_open_split,
         -- ["<c-t>"] = custom_actions.multi_selection_open_tab,
         },
      },
   },
   pickers = {
      find_files = {
         mappings = {
            i = {
               ["<cr>"] = custom_actions.multi_selection_open,
            },
            n = {
               ["<cr>"] = custom_actions.multi_selection_open,
            }
         }
      },
      buffers = {
         initial_mode = 'normal',

         show_all_buffers = true,
         sort_lastused = true,
         -- theme = "dropdown",

         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               mirror = true,
               prompt_position = 'top',
               width = 90,
               preview_height = 0.7,
            },
         },
         mappings = {
            i = {  -- inser mode
              ["<c-d>"] = "delete_buffer",
            },
            n = {  -- normal mode
               d = 'delete_buffer',
            },
         },
      },
      live_grep = {
         -- sorting_strategy = 'descending',
         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               mirror = true,
               preview_height = 0.4,
            },
         },
         mappings = {
            i = {
               ["<CR>"] = custom_actions.auto_multi_selection_open_qflist
            },
            n = {
               ["<CR>"] = custom_actions.auto_multi_selection_open_qflist
            }
         }
      },
      man_pages = {
         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               mirror = true,
               prompt_position = 'top',
               width = 90,
               preview_height = 0.4,
            },
         },
      },
      command_history = {
         prompt_title = '',
         -- preview_title = 'find string in open buffers...',
         results_title = 'Command History',

         --           
         --  卑 喝   
         prompt_prefix = " ",
         -- prompt_prefix = " ",
         -- prompt_prefix = " ",
         -- prompt_prefix = " ",

         sorting_strategy = 'descending',
         layout_strategy = "bottom_pane",
         layout_config = {
            prompt_position = 'bottom',
            height = 25,
            -- bottom_pane = {
            --    prompt_position = 'bottom',
            -- },
         },
         border = true,
         borderchars = {
            prompt =  { " ", " ", "─", " ", " ", " ", "─", "─" },
            results = { "─", " ", " ", " ", "─", "─", " ", " " },
            preview = { "─", " ", "─", "│", "┬", "─", "─", "╰" },
         },
         mappings = {
            i = {
               ['<CR>'] = 'edit_command_line',
            },
            n = {
               ['<CR>'] = 'edit_command_line',
            },
         },
      },
      search_history = {
         prompt_title = '',
         results_title = 'Search History',
         prompt_prefix = " ",
         sorting_strategy = 'descending',
         layout_strategy = "bottom_pane",
         layout_config = {
            prompt_position = 'bottom',
            height = 25,
         },
         border = true,
         borderchars = {
            prompt =  { " ", " ", "─", " ", " ", " ", "─", "─" },
            results = { "─", " ", " ", " ", "─", "─", " ", " " },
            preview = { "─", " ", "─", "│", "┬", "─", "─", "╰" },
         },
         mappings = {
            i = {
               ['<CR>'] = 'edit_search_line',
            },
            n = {
               ['<CR>'] = 'edit_search_line',
            },
         },
      },
      spell_suggest = {
         initial_mode = 'normal',
         layout_strategy = 'cursor',
         layout_config = {
            cursor = {
               height = 30,
               width = 40,
            }
         },
      },
      current_buffer_fuzzy_find = {
         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               width = 90,
               mirror = true,
               preview_height = 0.5,
            },
         },
         mappings = {
            i = {
               ["<CR>"] = custom_actions.auto_multi_selection_open_loclist
            },
            n = {
               ["<CR>"] = custom_actions.auto_multi_selection_open_loclist
            }
         }
      },
   },
   extensions = {
      -- Your extension configuration goes here:
      -- extension_name = {
      --   extension_config_key = value,
      -- }
      -- please take a look at the readme of the extension you want to configure
   }
}

------------------------------ Extensions --------------------------------------

telescope.load_extension('fzf')  -- use fzf module in C

local project_available, _ = pcall(require, 'project_nvim')
if project_available then telescope.load_extension('projects') end

-- telescope.load_extension('neoclip')

require('keybindings').telescope()

-- vim: fml=1
