" Colors {{{

" fg0        #e2cca9   bg0               #32302f
" fg1        #e2cca9   bg1               #3c3836
"                      bg2               #3c3836
" grey0      #7c6f64   bg3               #504945
" grey1      #928374   bg4               #504945
" grey2      #a89984   bg5               #665c54
"                      bg_current_word   #45403d
" aqua       #8bba7f   bg_diff_blue      #0f3a42
" blue       #80aa9e   bg_diff_green     #3d4220
" green      #b0b846   bg_diff_red       #472322
" orange     #f28534
" purple     #d3869b   bg_statusline1    #3c3836
" red        #f2594b   bg_statusline2    #46413e
" yellow     #e9b143   bg_statusline3    #5b534d
"                      bg_visual_blue    #404946
" bg_yellow  #e9b143   bg_visual_green   #424a3e
" bg_green   #b0b846   bg_visual_red     #543937
" bg_red     #db4740   bg_visual_yellow  #574833

" let config = gruvbox_material#get_configuration()
" let palette = gruvbox_material#get_palette(&background, config.palette)
" echo palette

" }}}
" Cursor {{{

highlight lCursor guifg=#282828 guibg=#e9b143
" hi Cursor   ctermfg=235 ctermbg=109 guifg=#282828 guibg=#80aa9e

" }}}
" Diagnostics {{{

" Undercurl
" " highlight! ErrorText   gui=undercurl guisp=#f2594b
" " highlight! WarningText gui=undercurl guisp=#e9b143
highlight! InfoText    gui=undercurl guisp=#b0b846
highlight! HintText    gui=undercurl guisp=#80aa9e

" " Background
" highlight! ErrorText   guibg=#543937
" highlight! WarningText guibg=#574833
" highlight! InfoText    guibg=#424a3e
" highlight! HintText    guibg=#404946


highlight! link InfoFloat Green
highlight! link HintFloat Blue

highlight! link DiagnosticSignInfo GreenSign
" highlight! link DiagnosticSignHint BlueSign
highlight! link DiagnosticSignHint AquaSign

if g:gruvbox_material_diagnostic_virtual_text == 'colored'
    highlight! link VirtualTextInfo Green
    highlight! link VirtualTextHint Blue
endif

" }}}
" Folds {{{

" #DDAE52 #e6b24c #e8ae3c #ECA825

highlight Folded guifg=#e3a84e guibg=None
highlight FoldColumn guifg=#e8ae3c
highlight link NormalFloat Pmenu

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
" LSP {{{

" highlight! LspReferenceText  guifg=none guibg=#a89984 gui=bold
" highlight! LspReferenceText  gui=underline

" highlight! LspReferenceText guibg=#423d3a
" highlight! link LspReferenceWrite LspReferenceText
" highlight! link LspReferenceRead  LspReferenceText

" highlight! link LspSagaLightBulbSign Orange
highlight! link LspSagaLightBulbSign Yellow

" }}}
" LSP signature {{{

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

" highlight! link NeorgDefinitionTitle AquaBold
highlight! link NeorgDefinitionTitle PurpleBold


" }}}
" Fidget {{{

" highlight FidgetTitle ctermfg=110 guifg=#6cb6eb
" highlight FidgetTitle guifg=#fa9041
highlight FidgetTitle guifg=#ffa059
" highlight link FidgetTask Comment
highlight FidgetTask guifg=#b8a78c

" }}}
" Dressing {{{

" highlight! link FloatTitle Grey
" highlight! FloatTitle guifg=#a89984
highlight! link FloatTitle Normal

" }}}
" Nvim Tree {{{

" highlight! link NvimTreeOpenedFile Blue

" }}}

" vim: fdm=marker
