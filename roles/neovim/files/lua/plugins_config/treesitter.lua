-- HACK This file is loaded after nvim-treesitter plugin.

local available, treesitter_config = pcall(require, "nvim-treesitter.configs")
if not available then return end

treesitter_config.setup {
   -- ensure_installed = "maintained",
   -- 'yaml'
   ensure_installed = {
      'bash', 'comment', 'c', 'cpp', 'fennel', 'go', 'html', 'json',
      'latex', 'lua', 'python', 'regex', 'rust', 'toml', 'query'
   },
   highlight = {
      enable = true,
      use_languagetree = true
   },
   indent = {enable = true},
   incremental_selection = {
      enable = true,
      keymaps = {
         init_selection = 'gnn',
         node_incremental = 'grn',
         scope_incremental = 'grc',
         node_decremental = 'grm'
      }
   },
   playground = {
      enable = true,
      disable = {},
      updatetime = 25, -- Debounced time for highlighting nodes in the playground
                       --  from source code.
      persist_queries = false, -- Whether the query persists across vim sessions.
      keybindings = {
         toggle_query_editor = 'o',
         toggle_hl_groups = 'i',
         toggle_injected_languages = 't',
         toggle_anonymous_nodes = 'a',
         toggle_language_display = 'I',
         focus_language = 'f',
         unfocus_language = 'F',
         update = 'R',
         goto_node = '<cr>',
         show_help = '?',
      },
   },
   textobjects = require("keybindings").treesitter_textobjects(),
   textsubjects = {
       enable = true,
       keymaps = {
           ['.'] = 'textsubjects-smart',
           [';'] = 'textsubjects-container-outer',
       }
   },
   rainbow = {
      enable = true,
      extended_mode = true, -- Highlight also non-parentheses delimiters,
                            -- boolean or table: lang -> boolean.
      max_file_lines = 1000, -- Do not enable for files with more than 1000 lines, int.
   }

   -- refactor = {
   --   smart_rename = {enable = true, keymaps = {smart_rename = "grr"}},
   --   highlight_definitions = {enable = true}
   --   -- highlight_current_scope = { enable = true }
   -- },

}
