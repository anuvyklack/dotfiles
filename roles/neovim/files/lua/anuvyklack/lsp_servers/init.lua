local lsp_util = require 'lspconfig'.util
local M = {}

M.sumneko_lua = require("lua-dev").setup {
   lspconfig = {
      root_dir = lsp_util.root_pattern('.root',
         '.luarc.json', '.luacheckrc', '.stylua.toml', 'selene.toml', '.git'),
      settings = {
         Lua = {
            IntelliSense = {
               traceLocalSet = true,
               traceReturn = true,
               traceBeSetted = true,
               traceFieldInject = true
            },
            diagnostics = {
               globals = { 'P', 'prequire', 'vim' },
            },
            completion = {
               showParams = false
            },
            format = {
               enable = true,
               defaultConfig = {
                  indent_style = "space",
                  indent_size = "3",
               }
            },
            runtime = {
               version = 'LuaJIT',
               path = vim.list_extend(vim.split(package.path, ';'), { "lua/?.lua", "lua/?/init.lua" }),
            },
            -- workspace = {
            --    -- Make the server aware of Neovim runtime files
            --    library = vim.api.nvim_get_runtime_file("", true),
            -- },
            telemetry = {
               -- Do not send telemetry data containing a randomized but unique identifier.
               enable = false,
            },
         },
      },
   }
}

M.ccls = {
   -- A list of ccls available options:
   -- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
   init_options = {
      -- compilationDatabaseDirectory = 'build';
      compilationDatabaseDirectory = 'build-Debug';
      index = {
         threads = 0
      }
   }
}

M.clangd = {
   cmd = {
      'clangd',
      -- '--compile-commands-dir=debug',
      '--background-index',
      '--suggest-missing-includes',
      '--pch-storage=memory',
      '--cross-file-rename',

      -- -- One completion item for each semantically distinct completion,
      -- -- with full type information.
      -- '--completion-style=detailed',

      -- Similar completion items (e.g. function overloads) are combined.
      -- Type information shown where possible.
      '--completion-style=bundled',
   },
}

M.cmake = {
   init_options = {
      buildDirectory = 'build-Debug'
   }
}

M.vimls = { root_dir = lsp_util.root_pattern('.git/', '.root') }

M.bashls = { filetypes = { 'sh', 'zsh' } }

M.ansiblels = {}
M.gopls = {}
M.pyright = {}

return M

