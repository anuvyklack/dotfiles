local api = vim.api
local prequire = require('util').prequire
local cmp = require 'cmp'
local cmp_types = require 'cmp.types'
local cmp_str = require 'cmp.utils.str'
local cmap = cmp.mapping
local luasnip = prequire 'luasnip'
local autopairs_cmp = prequire 'nvim-autopairs/completion/cmp'

local function has_words_before()
   local line, col = unpack(api.nvim_win_get_cursor(0))
   return col ~= 0 and api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local custom_mapping = {
   select_next = function(fallback)
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
   end,
   select_prev = function(fallback)
      if cmp.visible() then
         cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
         luasnip.jump(-1)
      else
         fallback()
      end
   end,
   preset = {
      cmdline = cmp.mapping.preset.cmdline({
         ['<C-n>'] = function(fallback) fallback() end,
         ['<C-p>'] = function(fallback) fallback() end,
      }),
   }
}

-- General ---------------------------------------------------------------------

cmp.setup {
   snippet = {
      expand = function(args)
         luasnip.lsp_expand(args.body)
      end,
   },
   window = {
      documentation = {
         max_width = 90
      }
   },
   mapping = {
      ['<Down>'] = cmap.select_next_item({ behavior = 'insert' }),
      ['<Up>']   = cmap.select_prev_item({ behavior = 'insert' }),

      ["<Tab>"]   = cmap(custom_mapping.select_next, {'i','s'}),
      ["<S-Tab>"] = cmap(custom_mapping.select_prev, {'i','s'}),

      ['<C-j>'] = cmap.select_next_item({ behavior = 'insert' }),
      ['<C-k>'] = cmap.select_prev_item({ behavior = 'insert' }),

      -- Accept currently selected item. Set "select" to "false" to only confirm
      -- explicitly selected items.
      ['<CR>'] = cmap.confirm({ select = true }),
      ['<C-y>'] = cmap.confirm({ select = false }),

      ['<C-e>'] = cmap.abort(),
      -- ['<C-Space>'] = cmap.abort(),
      -- ['<Esc>'] = cmap.abort(),

      -- Scroll docs windows.
      ['<C-d>'] = cmap.scroll_docs(4),
      ['<C-f>'] = cmap.scroll_docs(4),
      ['<C-b>'] = cmap.scroll_docs(-4),
      ['<C-u>'] = cmap.scroll_docs(-4),

      ['<C-l>'] = cmap(function(fallback)
         if cmp.visible() then
            return cmp.complete_common_string()
         end
         fallback()
      end, {'i','c'}),
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
      { name = 'path',
        option = {
           -- Specify if completed directory names should include a trailing
           -- slash. Enabling this option makes this source behave like Vim's
           -- built-in path completion.
           trailing_slash = false
        }
      },
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
      { name = 'nvim_lua' }, -- Neovim's Lua runtime API such 'vim.lsp.*'
   },{
      { name = 'buffer',
        option = {
           get_bufnrs = function()
              -- Use all buffers for completion.
              return api.nvim_list_bufs()
           end
        },
      },
      -- { name = "dictionary",
      --   keyword_length = 2,
      -- },
   }),

   formatting = {
      fields = {
         cmp.ItemField.Abbr,
         cmp.ItemField.Kind,
         cmp.ItemField.Menu,
      },
      format = require('lspkind').cmp_format {
         -- How annotations are shown:
         -- 'text', 'text_symbol', 'symbol_text', 'symbol'
         mode = 'symbol_text',
      },
   },
}

-- `/` cmdline setup.
cmp.setup.cmdline('/', {
   mapping = custom_mapping.preset.cmdline,
   sources = {
      { name = 'buffer' }  -- Use buffer source for `/`
   },
   formatting = {
      fields = {
         cmp.ItemField.Abbr,
      },
   }
})

-- `:` cmdline setup.
cmp.setup.cmdline(':', {
   mapping = custom_mapping.preset.cmdline,
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

-- To insert `(` after select function or method item
cmp.event:on('confirm_done', autopairs_cmp.on_confirm_done{
   map_char = {
      tex = ''
   }
})

-- Filetypes -------------------------------------------------------------------

cmp.setup.filetype('norg', {
   window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
   },
   formatting = {
      fields = {
         cmp.ItemField.Kind,
         cmp.ItemField.Abbr,
         cmp.ItemField.Menu,
      },
      format = require('lspkind').cmp_format({  -- lspkind icons
         with_text = false,
         before = function(entry, vim_item)
            -- Get the full snippet (and only keep first line)
            local word = entry:get_insert_text()
            if entry.completion_item.insertTextFormat == cmp_types.lsp.InsertTextFormat.Snippet
            then
               word = vim.lsp.util.parse_snippet(word)
            end
            word = cmp_str.oneline(word)

            -- concatenates the string
            -- local max = 50
            -- if string.len(word) >= max then
            --    local before = string.sub(word, 1, math.floor((max - 3) / 2))
            --    word = before .. "..."
            -- end

            if entry.completion_item.insertTextFormat == cmp_types.lsp.InsertTextFormat.Snippet
               and string.sub(vim_item.abbr, -1, -1) == "~"
            then
               word = word .. "~"
            end
            vim_item.abbr = word

            return vim_item
         end
      })
   },
   sources = cmp.config.sources({
      { name = 'path' },
      { name = 'neorg' }
   },{
      { name = 'buffer',
        option = {
           get_bufnrs = function()
              return vim.api.nvim_list_bufs()
           end
        },
      }
   }),
})
