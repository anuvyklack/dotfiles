vim.bo.textwidth   = 80
vim.bo.tabstop     = 3
vim.bo.softtabstop = 3
vim.bo.shiftwidth  = 3
-- vim.wo.foldmethod  = 'expr'

--------------------------------------------------------------------------------

-- Make 'gf' vim keybinding work on `lua requare('module.foo')` statements.
-- For this, we need to add `.lua` extension to search name. And add `lua/`
-- directory in '~/.config/nvim/lua' to path.
-- Taken from here:
-- https://www.reddit.com/r/vim/comments/apce2p/gf_for_lua/

-- Set in Neovim by default
-- vim.bo.suffixesadd = ".lua"  -- Resolves 'foo' as 'foo.lua'.

-- Taken from :help includeexpr
-- Substitute '.' with '/' to resolve 'modules.foo' as 'modules/foo'.
vim.bo.includeexpr = "substitute(v:fname, '\\.', '/', 'g')"

vim.bo.keywordprg = ":help"

local path = vim.opt_local.path
path:append( vim.fn.stdpath("config") .. "/lua" )

-- Keybindings -----------------------------------------------------------------

vim.keymap.set({'n','x'}, 'gK', 'K', { buffer = true, desc = 'Show :help' })

-- Ufo -------------------------------------------------------------------------

local lua_ts_handler = function(virt_text, lnum, end_lnum, available_width, truncate, ctx)
   -- local folded_sign = '⋯'
   local folded_sign = '…'
   -- local folded_sign = '...'

   -- print(vim.api.nvim_get_hl_id_by_name('luaTSComment'))
   -- P(virt_text)

   local original_text_with = ctx.text:len()
   local comment_hl = vim.api.nvim_get_hl_by_name('Comment', vim.o.termguicolors)
   local comment_tokens = { '--' }
   local fold_marker = vim.opt.foldmarker:get()[1]
   local lua_patterns = {
      empty = '^%s+$',
      fold_marker = table.concat{ '%s?', vim.pesc(fold_marker), '%d*' },
      only_comment_token = {}
   }
   for _, token in ipairs(comment_tokens) do
      token = vim.pesc(token)
      table.insert(
         lua_patterns.only_comment_token,
         table.concat{ '^%s*', token, '%s*$' }
      )
   end

   local new_virt_text = {}
   local cur_width = 0

   local function is_comment(chunk)
      local hl_id = chunk[2]
      if type(hl_id) == 'number' then
         local hl = vim.api.nvim_get_hl_by_id(hl_id, vim.o.termguicolors)
         return vim.deep_equal(hl, comment_hl)
      else
         return false
      end
   end

   local function is_empty(chunk)
      local text = chunk[1]
      if text:find(lua_patterns.empty) then
         return true
      else
         return false
      end
   end

   local function add_chunk(chunk)
      -- Remove fold marker from comment
      if is_comment(chunk) then
         chunk[1] = chunk[1]:gsub(lua_patterns.fold_marker, '')
         for _, pat in ipairs(lua_patterns.only_comment_token) do
            if chunk[1]:find(pat) then
               return true
            end
         end
      end

      local continue = true
      local chunk_text = chunk[1]
      local chunk_width = vim.fn.strdisplaywidth(chunk_text)
      if available_width > cur_width + chunk_width then
         table.insert(new_virt_text, chunk)
         continue = true
      else
         chunk_text = truncate(chunk_text, available_width - cur_width)
         chunk_width = vim.fn.strdisplaywidth(chunk_text)
         local hl_group = chunk[2]
         table.insert(new_virt_text, { chunk_text, hl_group })
         continue = false

         -- -- str width returned from truncate() may less than 2nd argument, need padding
         -- if cur_width + chunk_width < available_width then
         --    suffix = suffix..(' '):rep(available_width - cur_width - chunk_width)
         -- end

      end
      cur_width = cur_width + chunk_width
      return continue
   end

   local function add_end_virt_text()
      local continue = true
      for i, chunk in ipairs(ctx.end_virt_text) do
         if not (i == 1 and is_empty(chunk)) and
            not (i == #ctx.end_virt_text and is_comment(chunk))
         then
            continue = add_chunk(chunk)
            if not continue then break end
         end
      end
      return continue
   end

   local continue = true
   for i, chunk in ipairs(virt_text) do
      if i ~= #virt_text then
         continue = add_chunk(chunk)
      elseif #virt_text == 1 or not is_comment(chunk) then
         continue = add_chunk(chunk) and
                    add_chunk({' '..folded_sign..' ', 'Comment'}) and
                    add_end_virt_text()
      else
         continue = add_chunk({folded_sign..' ', 'Comment'}) and
                    add_end_virt_text() and
                    add_chunk({' ', 'UfoFoldedFg'}) and
                    add_chunk(chunk)
      end
      if not continue then break end
   end

   if cur_width < original_text_with then
      add_chunk({string.rep(' ', original_text_with - cur_width), 'UfoFoldedFg'})
   end

   -- if continue then
   --    local suffix = ('  %d '):format(end_lnum - lnum)
   --    continue =
   --       add_chunk({string.rep(' ', available_width - cur_width - 10), 'UfoFoldedFg'})
   --       and add_chunk({ suffix, 'MoreMsg' })
   -- end

   return new_virt_text
end

local prequire = require('util').prequire
prequire('ufo').setFoldVirtTextHandler(nil, lua_ts_handler)

