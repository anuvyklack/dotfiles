-- local lspconfig = require'lspconfig'
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the needed keys
-- after the language server attaches to the current buffer.
local on_attach = function(client, bufnr)
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    --Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- require('lsp_signature').on_attach()
    -- require('virtualtypes').on_attach()
    require 'illuminate'.on_attach(client)

    -- Load lsp keybindings.
    require("keybindings").lspconfig(bufnr)
end


-- -- Use a loop to conveniently call 'setup' on multiple servers and
-- -- map buffer local keybindings when the language server attaches
-- local servers = { "pyright" }
-- for _, lsp in ipairs(servers) do
--   nvim_lsp[lsp].setup { on_attach = on_attach }
-- end


-- A list of available options:
-- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
nvim_lsp.ccls.setup {
  on_attach = on_attach,
  -- Customization options are passed to ccls at
  -- initialization time via init_options.
  init_options = {
    -- compilationDatabaseDirectory = "build";
    compilationDatabaseDirectory = "build-Debug";
    index = {
      threads = 0;
    };
  }
}

-- nvim_lsp.pyright.setup{
--   on_attach = {
--     on_attach,
--     require('lsp_signature').on_attach  -- type hints
--   }
-- }

-- vim: ts=2 sts=2 sw=2 tw=80 cc=+1 fen fdm=marker
