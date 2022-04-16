vim.bo.textwidth = 90

vim.bo.tabstop     = 2
vim.bo.softtabstop = 2
vim.bo.shiftwidth  = 2

vim.opt.fillchars:append('fold: ')
vim.wo.foldminlines = 2    -- minimum lines required to create fold

-- local cmp_available, cmp = pcall(require, 'cmp')
-- if cmp_available then
--
--    local cmp_types = require("cmp.types")
--    local cmp_str = require("cmp.utils.str")
--
--    cmp.setup.buffer({
--       window = {
--          documentation = {
--             border = "rounded",
--             scrollbar = "║",
--          },
--          completion = {
--             border = "rounded",
--             scrollbar = "║",
--          },
--       },
--       formatting = {
--          fields = {
--             cmp.ItemField.Kind,
--             cmp.ItemField.Abbr,
--             cmp.ItemField.Menu,
--          },
--          format = require('lspkind').cmp_format({  -- lspkind icons
--             with_text = false,
--             before = function(entry, vim_item)
--                -- Get the full snippet (and only keep first line)
--                local word = entry:get_insert_text()
--                if entry.completion_item.insertTextFormat == cmp_types.lsp.InsertTextFormat.Snippet
--                then
--                   word = vim.lsp.util.parse_snippet(word)
--                end
--                word = cmp_str.oneline(word)
--
--                -- concatenates the string
--                -- local max = 50
--                -- if string.len(word) >= max then
--                --    local before = string.sub(word, 1, math.floor((max - 3) / 2))
--                --    word = before .. "..."
--                -- end
--
--                if entry.completion_item.insertTextFormat == cmp_types.lsp.InsertTextFormat.Snippet
--                   and string.sub(vim_item.abbr, -1, -1) == "~"
--                then
--                   word = word .. "~"
--                end
--                vim_item.abbr = word
--
--                return vim_item
--             end
--          })
--       },
--       sources = cmp.config.sources({
--          { name = 'nvim_lsp' },
--          { name = 'luasnip' },
--          { name = 'path' },
--          { name = 'nvim_lua' }, -- Neovim's Lua runtime API such 'vim.lsp.*'
--          { name = 'neorg' }
--       },{
--          { name = 'buffer',
--            option = {
--               get_bufnrs = function()
--                  return vim.api.nvim_list_bufs()
--               end
--            },
--          }
--       }),
--    })
--
-- end
