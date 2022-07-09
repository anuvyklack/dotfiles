" Set the executive for some filetypes explicitly. Use the explicit executive
" instead of the default one for these filetypes when using `:Vista` without
" specifying the executive.
let g:vista_executive_for = {
\	'cpp': 'vim_lsp',
\	'lua': 'nvim_lsp',
\ }
