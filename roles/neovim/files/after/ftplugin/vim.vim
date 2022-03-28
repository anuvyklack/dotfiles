setlocal foldmethod=expr    " use treesitter base folding

lua << EOF
-- local pretty_fold_available, pretty_fold = pcall(require, 'pretty-fold')
-- if pretty_fold_available then
--    pretty_fold.ft_setup('vim', {
--       fill_char = '+', -- use 'space' as a fold char
--    })
-- end
EOF
