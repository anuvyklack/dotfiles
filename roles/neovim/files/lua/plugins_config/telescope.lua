local available, telescope = pcall(require, "telescope")
if not available then return end

telescope.setup({
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
         },
         n = {  -- normal mode
            ["<Space>"] = 'toggle_selection',
         },
      },

   },
   pickers = {
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
      },
   },
   extensions = {
      -- Your extension configuration goes here:
      -- extension_name = {
      --   extension_config_key = value,
      -- }
      -- please take a look at the readme of the extension you want to configure
   }
})

telescope.load_extension('projects')
-- telescope.load_extension('neoclip')

require('keybindings').telescope()
