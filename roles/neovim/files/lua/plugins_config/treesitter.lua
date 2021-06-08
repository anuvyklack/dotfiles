-- HACK This file is loaded after nvim-treesitter plugin.

require('nvim-treesitter.configs').setup {

  ensure_installed = "maintained",

  -- ensure_installed = {
  --   'bash', 'bibtex', 'c', 'comment', 'cpp', 'css', 'fennel', 'go', 'haskell',
  --   'html', 'javascript', 'json', 'jsonc', 'julia', 'kotlin', 'latex', 'lua',
  --   'python', 'regex', 'rust', 'teal', 'toml', 'typescript', 'query', 'yaml',
  --   'zig'
  -- },

  highlight = { enable = true,
                use_languagetree = true },

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

  rainbow = {
    enable = true,
    -- Highlight also non-parentheses delimiters, 
    --   boolean or table: lang -> boolean
    extended_mode = true, 
    -- Do not enable for files with more than 1000 lines, int
    max_file_lines = 1000, 
  }

  -- refactor = {
  --   smart_rename = {enable = true, keymaps = {smart_rename = "grr"}},
  --   highlight_definitions = {enable = true}
  --   -- highlight_current_scope = { enable = true }
  -- },
}

-- print( require("keybindings").treesitter_textobjects() )
