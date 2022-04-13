local pretty_fold = require('pretty-fold')

vim.opt.fillchars:append('fold:•')

-- require('pretty-fold').setup()
pretty_fold.setup {
   global = {
      matchup_patterns = {
         { 'function%s*%(', 'end' }, -- 'function( or 'function (''
         {  '{', '}' },
         { '%(', ')' }, -- % to escape lua pattern char
         { '%[', ']' }, -- % to escape lua pattern char
      },
      -- ft_ignore = { 'vim' }
   },
   -- marker = { comment_signs = 'spaces' },
   marker = { process_comment_signs = 'spaces' },
   expr = { process_comment_signs = false }
}

-- Lua
pretty_fold.ft_setup('lua', {
   process_comment_signs = false ,
   matchup_patterns = {
      -- ╟─ Start of line ──╭───────╮── "do" ── End of line ─╢
      --                    ╰─ WSP ─╯
      { '^%s*do$', 'end' }, -- `do ... end` blocks

      -- ╟─ Start of line ──╭───────╮── "do" ──╭───────╮ ─╢
      --                    ╰─ WSP ─╯          ╰─ WSP ─╯
      { '^%s*do%s', 'end' }, -- `do ... end` blocks with comment

      -- ╟─ Start of line ──╭───────╮── "if" ─╢
      --                    ╰─ WSP ─╯
      { '^%s*if', 'end' },

      -- ╟─ Start of line ──╭───────╮── "for"─╢
      --                    ╰─ WSP ─╯
      { '^%s*for', 'end' },

      -- ╟─ "function" ──╭───────╮── "(" ─╢
      --                 ╰─ WSP ─╯
      { 'function%s*%(', 'end' }, -- 'function(' or 'function ('

      {  '{', '}' },
      { '%(', ')' }, -- % to escape lua pattern char
      { '%[', ']' }, -- % to escape lua pattern char
   }
})

-- C/C++
local cpp_fold_config = {
   -- fill_char = ' ', -- use 'space' as fold char
   process_comment_signs = false,
   stop_words = {
      -- ╟─ "*" ──╭──────────────╮── "@brief" ──╭──────────────╮──╢
      --          ╰─ whitespace ─╯              ╰─ whitespace ─╯
      '%*%s*@brief%s*',
   },
   comment_signs = {
      '/**'  -- C++ Doxygen comments
   }
}
pretty_fold.ft_setup('c', cpp_fold_config)
pretty_fold.ft_setup('cpp', cpp_fold_config)

require('pretty-fold/preview').setup()
