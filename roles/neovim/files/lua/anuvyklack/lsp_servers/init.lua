local lsp_util = require('lspconfig').util
local root_pattern = lsp_util.root_pattern
local M = {}

M.lua_ls = {
   root_dir = root_pattern('.root', '.luarc.json', '.luacheckrc',
      '.stylua.toml', 'selene.toml', '.git'),
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
         hint = {
            enable = true, -- inline hints
            arrayIndex = 'Disable', -- 'Enable', 'Auto', 'Disable'
            await = true,
            paramName = 'Disable', -- 'All', 'Literal', 'Disable'
            paramType = false,
            semicolon = 'Disable', -- 'All', 'SameLine', 'Disable'
            setType = true,
         },
         runtime = {
            version = "LuaJIT",
            -- pathStrict = false,
            -- -- path = vim.list_extend(vim.split(package.path, ';'), { "lua/?.lua", "lua/?/init.lua" }),
            -- path = {
            --    "meta/LuaJIT en-us utf8/?.lua",
            --    "meta/LuaJIT en-us utf8/?/init.lua",
            --    "library/?.lua",
            --    "library/?/init.lua",
            --    "lua/?.lua",
            --    "lua/?/init.lua",
            -- },
         },
         -- workspace = {
         --    -- Make the server aware of Neovim runtime files
         --    library = vim.api.nvim_get_runtime_file('', true),
         --    ignoreDir = { "types/stable" },
         -- },
         telemetry = {
            -- Do not send telemetry data containing a randomized but unique
            -- identifier.
            enable = false,
         },
      },
   },
}

-- M.ccls = {
--    -- A list of ccls available options:
--    -- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
--    init_options = {
--       -- compilationDatabaseDirectory = 'build';
--       compilationDatabaseDirectory = 'build-Debug';
--       index = {
--          threads = 0
--       }
--    }
-- }

M.cmake = {
   init_options = {
      buildDirectory = 'build-Debug'
   }
}

M.vimls = { root_dir = root_pattern('.git/', '.root') }

M.bashls = { filetypes = { 'sh', 'zsh' } }

M.ansiblels = {}
M.gopls = {}
M.pyright = {}
M.tsserver = {}

return M
