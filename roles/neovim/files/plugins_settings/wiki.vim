" let g:wiki_root = '~/wiki'
let g:wiki_root = '/mnt/d/artyu/wiki'

" List of filetypes for which wiki.vim should be enabled.
let g:wiki_filetypes = ['md', 'wiki']

" The default type of the link: wiki or md.
let g:wiki_link_target_type = 'md'

" A list of TODO toggles that may be toggled with <plug>(wiki-list-toggle),
" which is by default mapped to `<c-s>`.
let g:wiki_list_todos = ['TODO', 'DONE']

" The title of TOC listings.
let g:wiki_toc_title = 'Contents'
