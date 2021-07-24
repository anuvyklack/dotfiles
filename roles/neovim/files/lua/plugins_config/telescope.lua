local available, telescope = pcall(require, "telescope")
if not available then return end

telescope.setup{
   defaults = {
      prompt_prefix = " ",
      selection_caret = " ", --  卑 喝   
      dynamic_preview_title = true,
      sorting_strategy = 'ascending',

      -- layout_strategy = 'horizontal',
      -- layout_strategy = 'vertical',
      layout_strategy = 'flex',

      layout_config = {
         horizontal = {
            mirror = false,
            prompt_position = 'top',
            preview_width = 82,
         },
         vertical = {
            -- mirror = true,
            preview_height = 0.5,
         },
      },

      mappings = {
         i = { -- insert mode
            -- ["<esc>"] = require('telescope.actions').close,
            -- ["<C-n>"] = false, -- disable keymap

            -- ["<C-k>"] = require('telescope.actions').move_selection_better,
            ["<C-k>"] = 'move_selection_previous',
            ["<C-j>"] = 'move_selection_next',
         },
         n = {
            ["<Space>"] = 'toggle_selection',
         },
      },

   },
   pickers = {

      -- buffers = {
      --   show_all_buffers = true,
      --   sort_lastused = true,
      --   theme = "dropdown",
      --   previewer = false,
      --   mappings = {
      --     i = {
      --       ["<c-d>"] = "delete_buffer",
      --     },
      --     n = {
      --       D = "delete_buffer",
      --     }
      --   }
      -- },

      buffers = {
         -- The next option doesn't work:
         -- initial_mode = 'normal',
         -- Look: https://github.com/nvim-telescope/telescope.nvim/issues/750
         -- This is workaround:
         initial_mode = 'insert',
         on_complete = { function() vim.cmd"stopinsert" end },

         layout_strategy = 'vertical',
         layout_config = {
            vertical = {
               mirror = true,
               width = 90,
               preview_height = 0.7,
            },
         },
         mappings = {
            n = {
               D = 'delete_buffer',
            },
         },
      },

   },
}


-- local builtin = require('telescope.builtin')
-- local themes = require('telescope.themes')
-- builtin.find_files(themes.get_dropdown())

-- local builtin = require('telescope.builtin')
-- local themes = require('telescope.themes')
-- builtin.find_files(themes.get_ivy())


require('keybindings').telescope()
