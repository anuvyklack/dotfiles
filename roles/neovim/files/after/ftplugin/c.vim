set textwidth=80
set colorcolumn=+1      " Показывать ограничительную линию в следующей колонке
                        " после textwidth.

" Folds ----------------------------------------------------------------------
setlocal foldcolumn=3
" set foldnestmax=3   " deepest fold is 3 levels

setlocal foldmethod=expr " use treesitter base folding

lua << EOF
local pretty_fold_available, pretty_fold = pcall(require, 'pretty-fold')
if pretty_fold_available then
    require('pretty-fold').ft_setup('c', {
       -- fill_char = ' ', -- use 'space' as fold char
       process_comment_signs = false,
       stop_words = {
          -- ╟─ "*" ──╭───────╮── "@brief" ──╭───────╮──╢
          --          ╰─ " " ─╯              ╰─ " " ─╯
          --         ╭────────────────╮               ╭────────────────╮
          -- ╟─ "*" ─╯─╭ whitespace ╮─╰─ "@brief" ── ─╯─╭ whitespace ╮─╰──╢
          --           ╰──── * ─────╯                   ╰──── * ─────╯
          '%*%s*@brief%s*',
       },
       comment_signs = {
          '/**'  -- C++ Doxygen comments
       }
    })
end
EOF
