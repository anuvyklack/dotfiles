local ok, telescope = pcall(require, 'telescope')
if not ok then return end

local actions = require('telescope.actions')
local custom_actions = require('anuvyklack.telescope.custom_actions')

local layout_config = {
   flex = {
      flip_columns = 180,
   },
   horizontal = {
      width = 0.80,
      mirror = false,
      prompt_position = 'top',
      preview_width = { 0.5, min = 20 },
   },
   vertical = {
      -- width = 0.8,
      -- mirror = true,
      prompt_position = 'top',
      preview_height = 0.5,
   }
}

local config = {
   defaults = {
      prompt_prefix = '  ',
      selection_caret = ' ', --  卑 喝   
      dynamic_preview_title = true,
      sorting_strategy = 'ascending', -- or 'descending',
      scroll_strategy = 'limit', -- or 'cycle'
      layout_strategy = 'flex', -- 'flex', 'horizontal', 'vertical',
      layout_config = layout_config,
      set_env = { ['COLORTERM'] = 'truecolor' },
      -- border = {},
      -- borderchars = { " ", " ", " ", " ", " ", " ", " ", " " },
      vimgrep_arguments = {
         'rg',
         '--color=never',
         '--no-heading',
         '--with-filename',
         '--line-number',
         '--column',
         '--smart-case',
         '--hidden',
         '--glob=!.git/',
      },
      mappings = {
         -- ['<C-n>'] = false, -- disable keymap
         i = { -- insert mode
            ['<C-j>'] = actions.move_selection_next,
            ['<C-k>'] = actions.move_selection_previous,
            -- ['<C-k>'] = actions.move_selection_better,

            ['<C-p>'] = actions.cycle_history_prev,
            ['<C-n>'] = actions.cycle_history_next,

            ['<Tab>'] = actions.toggle_selection + actions.move_selection_next,
            ['<S-Tab>'] = actions.toggle_selection + actions.move_selection_previous,

            ['<C-q>'] = actions.smart_send_to_qflist + actions.open_qflist,

            -- ['<CR>'] = custom_actions.multi_selection_open,
            -- ['<C-v>'] = custom_actions.multi_selection_open_vsplit,
            -- ['<C-s>'] = custom_actions.multi_selection_open_split,
            -- ['<C-t>'] = custom_actions.multi_selection_open_tab,
         },
         n = { -- normal mode
            ['<Space>'] = actions.toggle_selection,
            ['q'] = actions.smart_send_to_qflist + actions.open_qflist,
         },
      },
   },
   pickers = {},
   extensions = {}
}

config.pickers.find_files = {
   -- https://github.com/nvim-telescope/telescope.nvim/issues/559
   hidden = false, -- show hidden files
   layout_config = {
      vertical = {
         mirror = true,
         preview_height = 0.5,
      },
   },
   -- mappings = {
   --    i = { ['<CR>'] = custom_actions.multi_selection_open, },
   --    n = { ['<CR>'] = custom_actions.multi_selection_open, }
   -- }
}

config.pickers.help_tags = {
   mappings = {
      i = {
         ['<CR>'] = actions.select_vertical
      },
      n = {
         ['<CR>'] = actions.select_vertical
      }
   }
}

config.pickers.buffers = {
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
      i = { ["<c-d>"] = "delete_buffer" },
      n = { d = 'delete_buffer' },
   }
}

config.pickers.man_pages = {
   layout_strategy = 'vertical',
   layout_config = {
      vertical = {
         mirror = true,
         prompt_position = 'top',
         width = 90,
         preview_height = 0.4,
      },
   }
}

config.pickers.command_history = {
   prompt_title = '',
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
      i = { ['<CR>'] = actions.edit_command_line },
      n = { ['<CR>'] = actions.edit_command_line },
   },
}

config.pickers.search_history = {
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
      i = { ['<CR>'] = actions.edit_search_line },
      n = { ['<CR>'] = actions.edit_search_line },
   },
}

config.pickers.spell_suggest = {
   initial_mode = 'normal',
   layout_strategy = 'cursor',
   layout_config = {
      cursor = {
         height = 30,
         width = 40,
      }
   },
}

config.pickers.current_buffer_fuzzy_find = {
   layout_strategy = 'vertical',
   layout_config = {
      vertical = {
         width = 90,
         -- mirror = true,
         preview_height = 0.5,
      },
   },
   mappings = {
      i = { ["<CR>"] = custom_actions.auto_multi_selection_open_loclist },
      n = { ["<CR>"] = custom_actions.auto_multi_selection_open_loclist }
   }
}

config.pickers.highlights = {
   layout_strategy = 'vertical',
   layout_config = {
      vertical = {
         width = 90,
         mirror = true,
         preview_height = 0.2,
      },
   },
}

config.pickers.builtin = {
   include_extensions = true -- show the pickers of the installed extensions
}

config.extensions["zf-native"] = {
   file = { -- options for sorting file-like items
      enable = true, -- override default telescope file sorter
      highlight_results = true, -- highlight matching text in results
      match_filename = true, -- enable zf filename match priority
   },
   generic = { -- options for sorting all other items
      enable = false, -- override default telescope generic item sorter
   }
}

-- config.extensions['project'] = {
--    base_dirs = {
--       vim.fn.stdpath('data') .. '/site/pack',
--       '~/code/neovim-plugins',
--    },
--    -- theme = "dropdown",
--    -- order_by = "asc",
-- }

telescope.setup(config)

telescope.load_extension('fzf') -- use fzf module in C
telescope.load_extension("zf-native")
telescope.load_extension('repo')
-- telescope.load_extension('project')
if pcall(require, 'neoclip') then
   telescope.load_extension('neoclip')
end
if pcall(require, 'yanky') then
   telescope.load_extension('yank_history')
end

-- https://github.com/nvim-telescope/telescope.nvim/issues/559
-- vim.cmd 'autocmd BufRead * autocmd BufWinEnter * ++once normal zxzM'
