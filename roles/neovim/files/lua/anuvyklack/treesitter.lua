local treesitter_config = require('nvim-treesitter.configs')

treesitter_config.setup {
   -- ensure_installed = { 'c', 'lua' },
   ensure_installed = 'all',
   ignore_install = { 'markdown', 'vim', 'help', 'yaml' },

   highlight = { enable = true },
   -- indent = {
   --    enable = true,
   --    disable = { 'python', 'yaml' }
   -- },
   incremental_selection = {
      enable = true,
      keymaps = {
         init_selection = "<S-Tab>", -- normal mode
         node_incremental = "<Tab>", -- visual mode
         node_decremental = "<S-Tab>", -- visual mode
      },
   },
   playground = {
      enable = true,
      disable = {},
      updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code.
      persist_queries = false, -- Whether the query persists across vim sessions.
   },
   textobjects = {
      select = {
         enable = true,
         lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim.
         keymaps = {
            -- You can use the capture groups defined in textobjects.scm
            ['af'] = '@function.outer',
            ['if'] = '@function.inner',
            -- ['aC'] = '@class.outer',
            -- ['iC'] = '@class.inner',
            ['ac'] = '@conditional.outer',
            ['ic'] = '@conditional.inner',
            -- ['ae'] = '@block.outer',
            -- ['ie'] = '@block.inner',
            ['al'] = '@loop.outer',
            ['il'] = '@loop.inner',
            ['is'] = '@statement.inner',
            ['as'] = '@statement.outer',
            ['am'] = '@call.outer',
            ['im'] = '@call.inner',
         }
      },
      move = {
         enable = true,
         set_jumps = true, -- whether to set jumps in the jumplist
         goto_next_start = {
            ["]m"] = "@function.outer",
            ["]]"] = "@class.outer",
         },
         -- goto_next_end = {
         --    ["]M"] = "@function.outer",
         --    ["]["] = "@class.outer",
         -- },
         goto_previous_start = {
            ["[m"] = "@function.outer",
            ["[["] = "@class.outer",
         },
         -- goto_previous_end = {
         --    ["[M"] = "@function.outer",
         --    ["[]"] = "@class.outer",
         -- },
      },
   },
   textsubjects = {
      enable = true,
      prev_selection = ',', -- select the previous selection
      keymaps = {
         ['.'] = 'textsubjects-smart',
         [';'] = 'textsubjects-container-outer',
         ['i;'] = 'textsubjects-container-inner',
      },
   },
   rainbow = {
      enable = true,
      extended_mode = true, -- Highlight also non-parentheses delimiters,
      -- boolean or table: lang -> boolean.
   },
   context_commentstring = {
      enable = true
   },
}
