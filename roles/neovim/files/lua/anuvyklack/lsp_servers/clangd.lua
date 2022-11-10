local Hydra, ok = prequire('hydra')
if not ok then return end
local telescope_pickers = require('anuvyklack/telescope/pickers')
local cmd = require('hydra.keymap-util').cmd

local function on_attach(client, bufnr)
   local hint = [[
 ^^                    ^^       Clangd            ^^      Telescope
 ^^-----------------   ^^----------------------   ^^-------------------- 
 _r_ rename            _/_ header <-> src         _td_ definitions
 _c_ code action       _s_ abstract syntax tree   _tr_ buffer references
 _S_ signature help    _i_ symbol info            _tR_ references
 _t_ type definition   _h_ type hierarchy         _ti_ implementation
 _f_ format            ^^                         _ts_ document symbols
 _v_ Vista             ^^                         _tS_ workspace symbols
]]

   Hydra {
      name = 'Clangd',
      hint = hint,
      config = {
         color = 'teal',
         buffer = bufnr,
         invoke_on_body = true,
         hint = {
            -- position = 'middle-right',
            border = 'rounded'
         }
      },
      mode = { 'n', 'x' },
      body = '<leader>l',
      heads = {
         { 'r', vim.lsp.buf.rename, { desc = 'rename' } },
         -- { 'r', prequire('renamer').rename, { desc = 'rename' } },

         { 'c', vim.lsp.buf.code_action, { desc = 'code action' } },

         { 'S', vim.lsp.buf.signature_help, { desc = 'sinature help' } },
         { 't', vim.lsp.buf.type_definition, { desc = 'type definition' } },
         { 'f', vim.lsp.buf.format, { desc = 'format' } },
         -- { 'f', function() vim.lsp.buf.format({ async = true }) end, { desc = 'format' } },

         { 'v', cmd 'Vista nvim_lsp', { desc = 'Vista' } },

         { '/', cmd 'ClangdSwitchSourceHeader', { desc = 'switch header / src' } },
         { 's', ':ClangdAST<CR>', { desc = 'Abstract Syntax Tree' } },
         { 'i', cmd 'ClangdSymbolInfo', { desc = 'symbol info' } },
         { 'h', cmd 'ClangdTypeHierarchy', { desc = 'type hierarchy' } },

         { 'td', telescope_pickers.definitions, { desc = 'definitions' } },
         { 'tr', telescope_pickers.buffer_references, { desc = 'buffer references' } },
         { 'tR', telescope_pickers.references, { desc = 'references' } },
         { 'ti', telescope_pickers.implementations, { desc = 'implementations' } },
         { 'ts', telescope_pickers.document_symbols, { desc = 'symbols' } },
         { 'tS', telescope_pickers.workspace_symbols, { desc = 'workspace symbols' } },

         { '<Esc>', nil, { exit = true, desc = false } }
      }
   }
end

prequire('clangd_extensions').setup {
   server = {
      on_attach = on_attach,
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
   },
   extensions = {
      autoSetHints = false,
      ast = {
         role_icons = {
            type = '',
            declaration = '',
            expression = '',
            specifier = '',
            statement = '',
            ['template argument'] = '',
         },
         kind_icons = {
            Compound = '',
            Recovery = '',
            TranslationUnit = '',
            PackExpansion = '',
            TemplateTypeParm = '',
            TemplateTemplateParm = '',
            TemplateParamObject = '',
         },
         highlights = {
            detail = 'Comment',
         },
      },
      memory_usage = {
         border = 'none',
      },
      symbol_info = {
         border = 'rounded',
      },
   }
}

