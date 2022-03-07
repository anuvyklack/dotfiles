" Colors {{{

" fg0:        #e2cca9
" fg1:        #e2cca9
" red:        #f2594b
" orange:     #f28534
" yellow:     #e9b143
" green:      #b0b846
" aqua:       #8bba7f
" blue:       #80aa9e
" purple:     #d3869b
" bg_red:     #db4740
" bg_green:   #b0b846
" bg_yellow:  #e9b143

" }}}
" Folds {{{

" #DDAE52 #e6b24c #e8ae3c #ECA825

hi Folded guifg=#e3a84e guibg=None

hi FoldColumn guifg=#e8ae3c

hi link NormalFloat Pmenu

" }}}
" Easymotion {{{

" Defaults:
" highlight EasyMotionTarget        guifg=#ff0000 gui=bold
" highlight EasyMotionTarget2First  guifg=#ffb400 gui=bold
" highlight EasyMotionTarget2Second guifg=#b98300

highlight EasyMotionTarget        guifg=#ff1414 gui=bold
highlight EasyMotionTarget2First  guifg=#ffb912 gui=bold
highlight EasyMotionTarget2Second guifg=#cf9200
" highlight! link EasyMotionShade Grey

" }}}
" Hop {{{

highlight HopNextKey   guifg=#ff1414 gui=bold
highlight HopNextKey1  guifg=#ffb912 gui=bold
highlight HopNextKey2  guifg=#e3a84e
" highlight HopNextKey2  guifg=#cf9200
" highlight HopUnmatched guifg=#aaaaaa

" highlight HopNextKey   guifg=#ff007c gui=bold
" highlight HopNextKey1  guifg=#00b7d1 gui=bold
" " highlight HopNextKey1  guifg=#0095a8 gui=bold
" highlight HopNextKey2  guifg=#00a3ba
" highlight HopUnmatched guifg=#aaaaaa

" }}}
" Lsp signature {{{

" highlight LspSignatureActiveParameter guifg=#ffb912 gui=bold
highlight! link LspSignatureActiveParameter WarningMsg

" }}}
" Color brackets {{{

" {'guifgs': ['#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'] }
" {'guifgs': ['#f28534', '#f2594b', '#80aa9e', '#e9b143', '#b0b846', '#d3869b'] }


lua << EOF
require'nvim-treesitter.configs'.setup {
   rainbow = {
      colors = {
         '#f28534', '#80aa9e', '#e9b143', '#b0b846', '#d3869b', '#f2594b'
      },
   },
}
EOF

"}}}
" Neorg {{{

highlight! link NeorgMarkupVerbatim Blue

highlight! link NeorgTodoItem1Pending Yellow
highlight! link NeorgTodoItem2Pending Yellow
highlight! link NeorgTodoItem3Pending Yellow
highlight! link NeorgTodoItem4Pending Yellow
highlight! link NeorgTodoItem5Pending Yellow
highlight! link NeorgTodoItem6Pending Yellow

" code blocks {{{

" highlight! NeorgTagBegin    guifg=#645a50
" highlight! NeorgTagEnd      guifg=#645a50
" highlight! NeorgTagNameWord guifg=#645a50

highlight! NeorgTagBegin    guifg=#5a5a5a
highlight! NeorgTagEnd      guifg=#5a5a5a
highlight! NeorgTagNameWord guifg=#5a5a5a

" highlight! NeorgTagBegin    guifg=#505050
" highlight! NeorgTagEnd      guifg=#505050
" highlight! NeorgTagNameWord guifg=#505050

" highlight! NeorgTagParameter guifg=#787e80
highlight! NeorgTagParameter guifg=#808080

" highlight! link NeorgTagParameter Grey
" highlight! NeorgTagParameter guifg=#c28d27
" highlight! NeorgTagParameter guifg=#b0a086 gui=bold

" }}}

highlight! NeorgDefinition    guifg=#595047
highlight! NeorgDefinitionEnd guifg=#595047

" highlight! NeorgDefinition    guifg=#5a5a5a
" highlight! NeorgDefinition    guifg=#4d4d4d

highlight! NeorgUnorderedList1 guifg=#80aa9e
highlight! NeorgUnorderedList2 guifg=#80aa9e
highlight! NeorgUnorderedList3 guifg=#80aa9e
highlight! NeorgUnorderedList4 guifg=#80aa9e
highlight! NeorgUnorderedList5 guifg=#80aa9e

" highlight! link NeorgDefinitionTitle Yellow
" highlight! link NeorgDefinitionTitle YellowBold
" highlight! link NeorgDefinitionTitle Purple


" }}}

" vim: fdm=marker
