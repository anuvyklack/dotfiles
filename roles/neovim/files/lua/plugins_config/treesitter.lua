-- HACK: This file is loaded after nvim-treesitter plugin.

local treesitter_config = require('nvim-treesitter.configs')

local config = {
   -- One of 'all', 'maintained' (parsers with maintainers), or a list of languages.
   -- ensure_installed = 'maintained',
   ensure_installed = {
      'lua', 'python', 'regex', 'comment', 'query', 'bash',
      'c', 'cpp', 'make', 'cmake', 'json', 'html', 'http', 'ninja', 'fennel',
      'go', 'gomod', 'gowork', 'latex', 'bibtex', 'rust', 'toml',
      'markdown',
      -- 'vim',
      -- 'yaml',
   },

   highlight = {
      enable = true, -- need for neorg

       -- Setting this to true will run `:h syntax` and tree-sitter at the same
       -- time.  Set this to `true` if you depend on 'syntax' being enabled
       -- (like for indentation).  Using this option may slow down your editor,
       -- and you may see some duplicate highlights.  Instead of true it can
       -- also be a list of languages.
       additional_vim_regex_highlighting = false,
   },
   indent = {
      enable = true,
      disable = {"python"}
   },
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
      -- disable = { -- List of languages you want to disable the plugin for.
      --    'jsx', 'cpp',
      -- },
      max_file_lines = 1000, -- Do not enable for files with more than 1000 lines, int.
   },
   context_commentstring = {
      enable = true
   },
   -- refactor = {
   --   smart_rename = {enable = true, keymaps = {smart_rename = "grr"}},
   --   highlight_definitions = {enable = true}
   --   -- highlight_current_scope = { enable = true }
   -- },
}

--------------------------------- Neorg ----------------------------------------
local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()

parser_configs.norg_meta = {
   install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg-meta",
      files = { "src/parser.c" },
      branch = "main"
   },
}
parser_configs.norg_table = {
   install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg-table",
      files = { "src/parser.c" },
      branch = "main"
   },
}

for _, parser in ipairs{ 'norg', 'norg_meta', 'norg_table' } do
   table.insert(config.ensure_installed, parser)
end
--------------------------------------------------------------------------------

treesitter_config.setup(config)
