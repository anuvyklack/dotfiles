-- List of manually installed language servers.
local manually_installed_servers = {
   -- "ccls"
}


local lspconfig = require('lspconfig')

local lspinstall_available, lspinstall = pcall(require, "lspinstall")
lspinstall.setup() -- Adds the missing :LspInstall <language> command.

-- Use an on_attach function to map the needed keys after

---@param bufnr number
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


local lsp_settings = {
   lua = require("lua-dev").setup({
      plugins = true,
      lspconfig = {
         -- cmd = {"lua-language-server"},
         on_attach = on_attach,
         settings = {
            Lua = {
               diagnostics = {
                  -- Get the language server to recognize the `use` global.
                  globals = {"use"},
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
   }),
   vim = {
      on_attach = on_attach,
   },
   cpp = { -- clangd
      on_attach = on_attach,
      cmd = {
         vim.env.HOME .. "/.local/share/nvim/lspinstall/cpp/clangd/bin/clangd",
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
      -- default_config = {
         init_options = {
            buildDirectory = "build-Debug"
         }
      -- },
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


local servers = {}
if lspinstall_available then
   servers = lspinstall.installed_servers()
end

-- if table is not empty
if next(manually_installed_servers) then
   -- Add the content of the 'manually_installed_servers'
   -- list to the 'servers' list.
   table.insert(servers, unpack(manually_installed_servers))
end


local function setup_lsp_servers()
   for _, server in ipairs(servers) do
      -- print(server)
      lspconfig[server].setup( lsp_settings[server] )
   end
end
setup_lsp_servers()

-- Automatically reload after `:LspInstall <server>`
-- so we don't have to restart neovim.
if lspinstall_available then
   lspinstall.post_install_hook = function()
      setup_lsp_servers()
      -- Triggers the FileType autocmd that starts the server.
      vim.cmd("bufdo e")
   end
end