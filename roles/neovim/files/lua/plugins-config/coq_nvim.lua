local autopairs_available, autopairs = pcall(require, 'nvim-autopairs')
local keymap = vim.keymap
local fn = vim.fn
local M = {}

function M.setup()
   vim.g.coq_settings = {
      auto_start = 'shut-up', -- skip the greeting message
      display = {
         icons = {
            mode = 'none',  -- do not show icons
            -- mode = 'short', -- show icons only
            -- mode = 'long', -- show icons + text
            spacing = 2
         },
         ghost_text = {
            enabled = true,
            context = {' ', ' '}
         },
         pum = {
            kind_context  = {" [", "]"},
            source_context = {'｢', '｣'}
         },
         preview = {
            border = 'rounded'
         }
      },
      keymap = {
         jump_to_mark = "" -- default: <c-h>
      },
   }

   if autopairs_available then
      -- Disable coq default keymapimgs.
      vim.g.coq_settings = vim.tbl_deep_extend(
         'keep',
         { keymap = { recommended = false } },
         vim.g.coq_settings
      )
   end
end

function M.config()
   -- vim.cmd('COQnow --shut-up') -- --shut-up : flag to skip the greeting message

   if autopairs_available then
      local opt = { expr = true }

      -- These mappings are coq recommended mappings unrelated to nvim-autopairs.
      keymap.set('i', '<esc>',   [[pumvisible() ? "<c-e><esc>" : "<esc>"]], opt)
      keymap.set('i', '<c-c>',   [[pumvisible() ? "<c-e><c-c>" : "<c-c>"]], opt)
      keymap.set('i', '<tab>',   [[pumvisible() ? "<c-n>"      : "<tab>"]], opt)
      keymap.set('i', '<s-tab>', [[pumvisible() ? "<c-p>"      : "<bs>"]], opt)

      local function CR()
         if fn.pumvisible() ~= 0 then
            if fn.complete_info({ 'selected' }).selected ~= -1 then
               return autopairs.esc('<c-y>')
            else
               return autopairs.esc('<c-e>') .. autopairs.autopairs_cr()
            end
         else
            return autopairs.autopairs_cr()
         end
      end

      local function BS()
         if fn.pumvisible() ~= 0 and
            fn.complete_info({ 'mode' }).mode == 'eval'
         then
            return autopairs.esc('<c-e>') .. autopairs.autopairs_bs()
         else
            return autopairs.autopairs_bs()
         end
      end

      keymap.set('i', '<cr>', CR, opt)
      keymap.set('i', '<bs>', BS, opt)
   end

   require('coq_3p') {
      { src = 'nvimlua', short_name = 'nLUA' }, -- Nvim Lua API
      { src = 'vimtex',  short_name = 'vTEX' },
      { src = 'copilot', short_name = 'COP', accept_key = '<c-f>' },
   }
end

return M

-- vim: foldenable foldnestmax=3
