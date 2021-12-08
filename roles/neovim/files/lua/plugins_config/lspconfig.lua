local lsp_installer = require("nvim-lsp-installer")

-- Use an on_attach function to map the needed keys after
local function on_attach (client, bufnr)

   local function buf_set_option(...)
      vim.api.nvim_buf_set_option(bufnr, ...)
   end

   -- Enable completion triggered by <c-x><c-o>
   buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

   require('lsp_signature').on_attach({
      -- Array of extra characters that will trigger signature completion.
      extra_trigger_chars = {"(", ","}
   })

   -- require('virtualtypes').on_attach()
   require('illuminate').on_attach(client)

   -- Load lsp keybindings.
   require("keybindings").lspconfig(bufnr)
end

local lsputil = require'lspconfig'.util
local lsp_settings = {
   sumneko_lua = require("lua-dev").setup{
      plugins = true,
      lspconfig = {
         root_dir = lsputil.root_pattern('.git/', '.root'),
         on_attach = on_attach,
         settings = {
            Lua = {
               diagnostics = {
                  -- Get the language server to recognize the `use` global.
                  globals = { 'use', 'map', 'nnoremap', 'vnoremap' },
               },
               completion = {
                  showParams = false
               },
               -- runtime = {
               --     -- Tell the language server which version of Lua you're
               --     -- using (most likely LuaJIT in the case of Neovim).
               --     version = 'LuaJIT',
               --     -- Setup your lua path
               --     path = vim.split(package.path, ';'),
               -- },
               -- workspace = {
               --     -- Make the server aware of Neovim runtime files
               --     library = vim.api.nvim_get_runtime_file("", true),
               -- },
               -- -- Do not send telemetry data containing a randomized but unique
               -- -- identifier.
               -- telemetry = {
               --     enable = false,
               -- },
            }
         }
      }
   },
   vimls = {
      root_dir = lsputil.root_pattern('.git/', '.root'),
      on_attach = on_attach,
   },
   clangd = {
      on_attach = on_attach,
      cmd = {
         vim.fn.stdpath('data').."/lsp_servers/clangd/clangd",
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
   ccls = {
      -- A list of ccls available options:
      -- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
      on_attach = on_attach,
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
   cmake = {
      on_attach = on_attach,
      init_options = {
         buildDirectory = "build-Debug"
      }
   },
   pyright = {
      on_attach = {
         on_attach,
         -- require('lsp_signature').on_attach  -- type hints
      }
   },
   bash = {
      on_attach = on_attach,
      filetypes = { "sh", "zsh" }
   }
}

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances
-- instead (see example below).
lsp_installer.on_server_ready(function(server)
    -- print(server.name)

    -- (optional) Customize the options passed to the server
    -- if server.name == "tsserver" then
    --     opts.root_dir = function() ... end
    -- end

    -- This setup() function is exactly the same as lspconfig's setup function.
    -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
    -- server:setup(opts)
    server:setup( lsp_settings[server.name] )
end)
