local api = vim.api

local available_cmp, cmp = pcall(require, 'cmp')
local available_luasnip, luasnip = pcall(require, 'luasnip')
if not available_cmp and not available_luasnip then return end

local function has_words_before()
   local line, col = unpack(api.nvim_win_get_cursor(0))
   return col ~= 0 and api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local function tab(fallback)
   if cmp.visible() then
      cmp.select_next_item()
   -- elseif luasnip.expand_or_jumpable() then
   elseif luasnip.expand_or_locally_jumpable() then
      luasnip.expand_or_jump()
   elseif has_words_before() then
      cmp.complete()
   else
      fallback()
   end
end

local function shift_tab(fallback)
   if cmp.visible() then
      cmp.select_prev_item()
   elseif luasnip.jumpable(-1) then
      luasnip.jump(-1)
   else
      fallback()
   end
end

cmp.setup {
   snippet = {
      expand = function(args)
         luasnip.lsp_expand(args.body) -- For `luasnip` users.
      end,
   },
   mapping = {
      ['<C-p>'] = cmp.mapping.select_prev_item(),
      ['<C-n>'] = cmp.mapping.select_next_item(),

      ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), {'i','c'}),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), {'i','c'}),
      ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), {'i','c'}),

      -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-y>'] = cmp.config.disable,

      -- ['<Esc>'] = cmp.mapping({
      ['<C-e>'] = cmp.mapping({
         i = cmp.mapping.abort(),
         c = cmp.mapping.close(),
      }),

      -- Accept currently selected item. Set `select` to `false`
      -- to only confirm explicitly selected items.
      ['<CR>'] = cmp.mapping.confirm({ select = true }),

      ["<Tab>"] = cmp.mapping(tab, {'i','s'}),
      ["<S-Tab>"] = cmp.mapping(shift_tab, {'i','s'}),
   },

   -- If you are interested in why sources are separated into two groups - this
   -- is responding for splitting sources in several groups according to their
   -- priority: if you don't want to see the buffer source items when the
   -- nvim-lsp source is available, place nvim-lsp source in a higher group
   -- relative to buffer source.
   -- See:
   --   :help cmp-config.sources
   --   :help cmp-config.sources[n].group_index
   sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
      { name = 'path' },
      { name = 'nvim_lua' }, -- Neovim's Lua runtime API such 'vim.lsp.*'
   },{
      { name = 'buffer',
        option = {
           get_bufnrs = function()
              return api.nvim_list_bufs()
           end
        },
      }
   }),

   completion = {
      border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
      scrollbar = "║"
   },
   window = {
      documentation = {
         border = "rounded",
         scrollbar = "║",
      },
      completion = {
         border = "rounded",
         scrollbar = "║",
      },
   },
   formatting = {
      fields = {
         cmp.ItemField.Abbr,
         cmp.ItemField.Kind,
         cmp.ItemField.Menu,
      },
      format = require('lspkind').cmp_format({  -- lspkind icons
         with_text = false, -- enables text annotations

         -- Prevent the popup from showing more than provided characters.
         -- (e.g 50 will not show more than 50 characters)
         maxwidth = 50,

         -- menu = ({
         --    buffer   = "｢Buffer｣",
         --    nvim_lsp = "｢LSP｣",
         --    luasnip  = "｢LuaSnip｣",
         --    nvim_lua = "｢Lua｣",
         --    latex_symbols = "｢Latex｣",
         -- })

         -- -- The function below will be called before any actual modifications
         -- -- from lspkind so that you can provide more controls on popup customization.
         -- -- See [#30](https://github.com/onsails/lspkind-nvim/pull/30)
         -- before = function (entry, vim_item)
         --    ...
         --    return vim_item
         -- end
      })
   },
}

-- Use buffer source for `/`
-- (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
   sources = {
      { name = 'buffer' }
   }
})

-- Use cmdline & path source for ':'
-- (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
   sources = cmp.config.sources({
      { name = 'path' }
   },{
      { name = 'cmdline' }
   }),
   formatting = {
      fields = {
         cmp.ItemField.Abbr,
      },
   }
})

local available_autopairs, autopairs_cmp = pcall(require, 'nvim-autopairs.completion.cmp')
if available_autopairs then
   -- If you want insert `(` after select function or method item
   cmp.event:on( 'confirm_done', autopairs_cmp.on_confirm_done{
      map_char = {
         tex = ''
      }
   })
end

-- vim: fml=3 foldnestmax=3
