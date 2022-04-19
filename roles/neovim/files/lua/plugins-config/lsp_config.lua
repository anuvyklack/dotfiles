local lsp_installer = require("nvim-lsp-installer")
local lsputil = require("lspconfig").util
local available_virtypes, virtualtypes = pcall(require, 'virtualtypes')
local available_illuminate, illuminate = pcall(require, 'illuminate')

local capabilities = vim.lsp.protocol.make_client_capabilities()
local available_cmp, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if available_cmp then
   capabilities = cmp_nvim_lsp.update_capabilities( capabilities )
end

-- Use an on_attach function to map the needed keys after
local function on_attach(client, bufnr)
   vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'
   vim.bo[bufnr].formatexpr = 'v:lua.vim.lsp.formatexpr()'

   if available_virtypes then
      virtualtypes.on_attach()
   end

   if available_illuminate then
      illuminate.on_attach(client)
   end

   -- Load lsp keybindings.
   require("keybindings").lspconfig(bufnr)
end


local runtime_path = vim.list_extend(
   vim.split(package.path, ';'), { "lua/?.lua", "lua/?/init.lua" })

local lua_config = {
   luadev = require("lua-dev").setup({
      lspconfig = {
         root_dir = lsputil.root_pattern('.root', '.git/'),
         on_attach = on_attach,
         capabilities = capabilities,
         settings = {
            Lua = {
               IntelliSense = {
                  traceLocalSet = true,
                  traceReturn = true,
                  traceBeSetted = true,
                  traceFieldInject = true
               },
               diagnostics = {
                  -- Get the language server to recognize the `use` global.
                  globals = { 'use' },
                  -- neededFileStatus = {
                  --    ["codestyle-check"] = "Any",
                  -- },
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
                   -- Tell the language server which version of Lua you're
                   -- using (most likely LuaJIT in the case of Neovim).
                   version = 'LuaJIT',
                   -- Setup your lua path
                   path = runtime_path,
               },
               workspace = {
                   -- Make the server aware of Neovim runtime files
                   library = vim.api.nvim_get_runtime_file("", true),
               },
               telemetry = {
                  -- Do not send telemetry data containing a randomized
                  -- but unique identifier.
                   enable = false,
               },
            }
         }
      }
   }),
   manual = {
      root_dir = lsputil.root_pattern('.root', '.git/')  or bufdir,
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
         Lua = {
            runtime = {
               -- Tell the language server which version of Lua you're using
               -- (most likely LuaJIT in the case of Neovim).
               version = 'LuaJIT',
               path = runtime_path, -- Setup your lua path
            },
            diagnostics = {
               -- Get the language server to recognize the `vim` global.
               globals = { 'vim', 'use' },
            },
            workspace = {
               -- Make the server aware of Neovim runtime files
               library = vim.api.nvim_get_runtime_file("", true),
            },
            telemetry = {
               -- Do not send telemetry data containing a randomized
               -- but unique identifier.
               enable = false,
            },
         },
      },
   }
}

local lsp_settings = {
   sumneko_lua = lua_config.luadev,
   -- sumneko_lua = lua_config.manual,
   ansiblels = {
      on_attach = on_attach,
      capabilities = capabilities,
   },
   ccls = {
      -- A list of ccls available options:
      -- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
      on_attach = on_attach,
      capabilities = capabilities,
      -- Customization options are passed to ccls at
      -- initialization time via init_options.
      init_options = {
         -- compilationDatabaseDirectory = "build";
         compilationDatabaseDirectory = "build-Debug";
         index = {
             threads = 0
         }
      }
   },
   clangd = {
      on_attach = on_attach,
      capabilities = capabilities,
      cmd = {
         "clangd",
         -- "--compile-commands-dir=debug",
         "--background-index",
         "--suggest-missing-includes",
         "--pch-storage=memory",
         "--cross-file-rename",
         -- "--completion-style=detailed", -- One completion item for each semantically
         --                                -- distinct completion, with full type information.
         "--completion-style=bundled", -- Similar completion items (e.g. function
                                       -- overloads) are combined. Type information
                                       -- shown where possible.
      },
   },
   cmake = {
      on_attach = on_attach,
      capabilities = capabilities,
      init_options = {
         buildDirectory = "build-Debug"
      }
   },
   gopls = {
      on_attach = on_attach,
      capabilities = capabilities,
   },
   pyright = {
      on_attach = {
         on_attach,
         -- require('lsp_signature').on_attach  -- type hints
      },
      capabilities = capabilities,
   },
   vimls = {
      root_dir = lsputil.root_pattern('.git/', '.root'),
      on_attach = on_attach,
      capabilities = capabilities,
   },
   bash = {
      on_attach = on_attach,
      capabilities = capabilities,
      filetypes = { "sh", "zsh" }
   }
}

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances
-- instead (see example below).
lsp_installer.on_server_ready(function(server)
   -- print(server.name)

   -- This setup() function is exactly the same as lspconfig's setup function.
   -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
   server:setup( lsp_settings[server.name] )
end)

-- vim: fml=3
