" Highlighting of class scope (область действия класса)
let g:cpp_class_scope_highlight = 1

" Highlighting of member variables
let g:cpp_member_variable_highlight = 1

" Highlighting of class names in declarations
let g:cpp_class_decl_highlight = 1

" Highlighting of POSIX functions
let g:cpp_posix_standard = 1

" Highlight template functions
" works in most cases, but can be a little slow on large files
" let g:cpp_experimental_simple_template_highlight = 1
" Alternatively set
let g:cpp_experimental_template_highlight = 1
" which is a faster implementation but has some corner cases where it doesn't work.

" Highlighting of library concepts.
" This will highlight the keywords `concept` and `requires` as well as all
" named requirements (like `DefaultConstructible`) in the standard library.
let g:cpp_concepts_highlight = 1

