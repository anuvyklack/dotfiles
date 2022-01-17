set textwidth=80
set colorcolumn=+1      " Показывать ограничительную линию в следующей колонке
                        " после textwidth.

" Folds ----------------------------------------------------------------------
setlocal foldcolumn=3
" set foldnestmax=3   " deepest fold is 3 levels

" Use treesitter base folding.
setlocal foldmethod=expr
setlocal foldexpr=nvim_treesitter#foldexpr()

lua << EOF
require('pretty-fold').ft_setup('cpp', {
   -- fill_char = ' ', -- use 'space' as fold char
   process_comment_signs = false,
   stop_words = {
      -- '%*%s*@brief%s*', -- 1 or more spaces -> '*' -> 1 or more spaces
      --                   -- -> '@brief' -> all spaces after
      '%s%*',      -- a space and star char
      '@brief%s*', -- '@brief' and any number of spaces after
   },
   comment_signs = {
      '/**', -- C++ Doxygen comments
   }
})
EOF
