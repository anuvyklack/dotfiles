local ok, lspconfig = pcall(require, 'lspconfig')
if not ok then return end

local prequire = require('util').prequire

local lsp_util = require 'lspconfig.util'
local null_ls = prequire 'null-ls'
local cmd_lsp_available, cmp_lsp = pcall(require, 'cmp_nvim_lsp')

prequire('nvim-lsp-installer').setup {
   -- automatic_installation = vim.fn.hostname() == 'zarathustra-huawei',
   ui = {
      check_outdated_servers_on_open = false,
      icons = {
         server_installed = '',
         server_pending = '',
         server_uninstalled = '',
      }
   }
}

---@param opts table|nil
local function create_capabilities(opts)
   opts = opts or { with_snippet_support = true }

   local capabilities = vim.lsp.protocol.make_client_capabilities()

   capabilities.textDocument.completion.completionItem.snippetSupport = opts.with_snippet_support
   if opts.with_snippet_support then
      capabilities.textDocument.completion.completionItem.resolveSupport = {
         properties = { 'documentation', 'detail', 'additionalTextEdits', }
      }
   end

   -- For "kevinhwang91/nvim-ufo".
   -- Nvim hasn't added foldingRange to default capabilities, so tell the server
   -- the capability of "foldingRange".
   capabilities.textDocument.foldingRange = {
      dynamicRegistration = false,
      lineFoldingOnly = true
   }

   if cmd_lsp_available then
      capabilities = cmp_lsp.update_capabilities(capabilities)
   end

   return capabilities
end

---@param bufnr number
local function buf_autocmd_codelens(bufnr)
   local augroup = vim.api.nvim_create_augroup("lsp_document_codelens", {})
   vim.api.nvim_create_autocmd({
     "BufEnter", "InsertLeave", "BufWritePost", "CursorHold"
   },{
      buffer = bufnr,
      group = augroup,
      callback = vim.lsp.codelens.refresh
   })
end

-- Finds and runs the closest codelens (searches upwards only)
local function find_and_run_codelens()
   local bufnr = vim.api.nvim_get_current_buf()
   local row, col = unpack(vim.api.nvim_win_get_cursor(0))
   local lenses = vim.lsp.codelens.get(bufnr)

   lenses = vim.tbl_filter(function(lense)
      return lense.range.start.line < row
   end, lenses)

   if #lenses == 0 then
      return vim.notify "Could not find codelens to run."
   end

   table.sort(lenses, function(a, b)
      return a.range.start.line > b.range.start.line
   end)

   vim.api.nvim_win_set_cursor(0, {
      lenses[1].range.start.line + 1,
      lenses[1].range.start.character
   })
   vim.lsp.codelens.run()
   vim.api.nvim_win_set_cursor(0, { row, col }) -- restore cursor, TODO: also restore position
end

local function common_on_attach(client, bufnr)
   vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'
   vim.bo[bufnr].formatexpr = 'v:lua.vim.lsp.formatexpr()'

   require('keybindings').lsp(bufnr)

   if client.config.flags then
      client.config.flags.allow_incremental_sync = true
   end

   if client.supports_method 'textDocument/documentHighlight' then
      prequire 'illuminate'.on_attach(client)
   end

   if client.supports_method "textDocument/codeLens" then
      buf_autocmd_codelens(bufnr)
      vim.schedule(vim.lsp.codelens.refresh)
   end
end

lsp_util.on_setup = lsp_util.add_hook_after(lsp_util.on_setup, function(config)
   -- if config.on_attach then
   --    config.on_attach = lsp_util.add_hook_after(config.on_attach, common_on_attach)
   -- else
   --    config.on_attach = common_on_attach
   -- end
   config.on_attach = lsp_util.add_hook_after(config.on_attach, common_on_attach)
   config.capabilities = create_capabilities()
end)

for server, config in pairs(require('anuvyklack/lsp_servers')) do
   lspconfig[server].setup(config)
end

null_ls.setup {
   sources = {
      null_ls.builtins.formatting.prettierd,
      null_ls.builtins.formatting.stylua,
      null_ls.builtins.diagnostics.shellcheck,
   },
   on_attach = common_on_attach,
}

