" Colors			{{{

" fg0		 #e2cca9   bg0				 #32302f
" fg1		 #e2cca9   bg1				 #3c3836
" grey0		 #7c6f64   bg3				 #504945
" grey1		 #928374   bg5				 #665c54
" grey2		 #a89984   bg_current_word	 #45403d
"					   bg_diff_blue		 #0f3a42
" aqua		 #8bba7f   bg_diff_green	 #3d4220
" blue		 #80aa9e   bg_diff_red		 #472322
" green		 #b0b846
" orange	 #f28534   bg_statusline1	 #3c3836
" purple	 #d3869b   bg_statusline2	 #46413e
" red		 #f2594b   bg_statusline3	 #5b534d
" yellow	 #e9b143   bg_visual_blue	 #404946
"					   bg_visual_green	 #424a3e
" bg_yellow  #e9b143   bg_visual_red	 #543937
" bg_green	 #b0b846   bg_visual_yellow  #574833
" bg_red	 #db4740

" let config = gruvbox_material#get_configuration()
" let palette = gruvbox_material#get_palette(&background, config.palette)
" echo palette

" }}}
" Built-in			{{{

" highlight! @punctuation.delimiter	guifg=#e2cca9

highlight! StatusLine	guifg=#e2cca9	guibg=#3c3836	gui=bold

highlight! WinBar		guifg=#d6c19f	guibg=#383432	gui=bold
" highlight! WinBar		guifg=#bfac8e	guibg=#383432	gui=bold

" highlight! WinSeparator guifg=#a89984
highlight! WinSeparator	guifg=#928374

highlight! SignColumn					guibg=#32302f
highlight! FoldColumn	guifg=#e97a6a	guibg=#32302f
highlight! LineNr		guifg=#7c6f64	guibg=#32302f

" }}}
" Cursor			{{{

highlight lCursor guifg=#282828 guibg=#e9b143
" hi Cursor   ctermfg=235 ctermbg=109 guifg=#282828 guibg=#80aa9e

" }}}
" Diagnostics		{{{

" " Undercurl
" " " highlight! ErrorText   gui=undercurl guisp=#f2594b
" " " highlight! WarningText gui=undercurl guisp=#e9b143
" highlight! InfoText    gui=undercurl guisp=#b0b846
" highlight! HintText    gui=undercurl guisp=#80aa9e
"
" " " Background
" " highlight! ErrorText	 guibg=#543937
" " highlight! WarningText guibg=#574833
" " highlight! InfoText	 guibg=#424a3e
" " highlight! HintText	 guibg=#404946
"
"
" highlight! link InfoFloat Green
" highlight! link HintFloat Blue
"
" highlight! link DiagnosticSignInfo GreenSign
" " highlight! link DiagnosticSignHint BlueSign
" highlight! link DiagnosticSignHint AquaSign
"
" if g:gruvbox_material_diagnostic_virtual_text == 'colored'
" 	highlight! link VirtualTextInfo Green
" 	highlight! link VirtualTextHint Blue
" endif

" }}}
" Folds				{{{

" " #DDAE52 #e6b24c #e8ae3c #ECA825
"
" " highlight Folded guibg=#2e2f3b
"
" " green
" " highlight Folded guibg=#424a3e
"
" " blue
" " highlight Folded guibg=#404946
"
" " highlight Folded guibg=#3e391a
" " highlight Folded guibg=#43334c
"
" " highlight Folded guibg=#473f3d
"
" " highlight Folded guibg=#40383c
" highlight Folded guibg=#3f3736
"
" " -- Cool --------------------------------
" " highlight Folded guibg=#233b4d
" " highlight Folded guibg=#3d383e
" " ----------------------------------------
"
" " highlight FoldColumn guifg=#e97a6a
"
" " highlight link NormalFloat Pmenu

" }}}
" Illuminate		{{{

" " highlight LspReferenceText	guibg=#4e3f54
" " highlight LspReferenceText	guibg=#4a4343
" highlight link LspReferenceText		Substitute
"
" highlight link LspReferenceWrite	LspReferenceText
" highlight link LspReferenceRead		LspReferenceText

" }}}
" Easymotion		{{{

" " Defaults:
" " highlight EasyMotionTarget		guifg=#ff0000 gui=bold
" " highlight EasyMotionTarget2First	guifg=#ffb400 gui=bold
" " highlight EasyMotionTarget2Second guifg=#b98300
"
" highlight EasyMotionTarget		  guifg=#ff1414 gui=bold
" highlight EasyMotionTarget2First  guifg=#ffb912 gui=bold
" highlight EasyMotionTarget2Second guifg=#cf9200
" " highlight! link EasyMotionShade Grey

" }}}
" Hop				{{{

highlight HopNextKey   guifg=#ff1414 gui=bold
highlight HopNextKey1  guifg=#ffb912 gui=bold
highlight HopNextKey2  guifg=#e3a84e
" highlight HopNextKey2  guifg=#cf9200
" highlight HopUnmatched guifg=#aaaaaa

" highlight HopNextKey	 guifg=#ff007c gui=bold
" highlight HopNextKey1  guifg=#00b7d1 gui=bold
" " highlight HopNextKey1  guifg=#0095a8 gui=bold
" highlight HopNextKey2  guifg=#00a3ba
" highlight HopUnmatched guifg=#aaaaaa

" }}}
" LSP				{{{

" " highlight! LspReferenceText  guifg=none guibg=#a89984 gui=bold
" " highlight! LspReferenceText  gui=underline
"
" " highlight! LspReferenceText guibg=#423d3a
" " highlight! link LspReferenceWrite LspReferenceText
" " highlight! link LspReferenceRead	LspReferenceText
"
" " highlight! link LspSagaLightBulbSign Orange
" highlight! link LspSagaLightBulbSign Yellow

" }}}
" LSP signature		{{{

" " highlight LspSignatureActiveParameter guifg=#ffb912 gui=bold
" highlight! link LspSignatureActiveParameter WarningMsg

" }}}
" Fidget			{{{

" " highlight FidgetTitle ctermfg=110 guifg=#6cb6eb
" " highlight FidgetTitle guifg=#fa9041
" highlight FidgetTitle guifg=#ffa059
" " highlight link FidgetTask Comment
" highlight FidgetTask guifg=#b8a78c

" }}}
" Dressing			{{{

" " highlight! link FloatTitle Grey
" " highlight! FloatTitle guifg=#a89984
" highlight! link FloatTitle Normal

" }}}
" Nvim Tree			{{{

" highlight! link NvimTreeOpenedFile Blue

" }}}
" vim-visual-multi	{{{

" " https://github.com/sainnhe/gruvbox-material/issues/131
" let g:VM_Mono_hl = 'Cursor'
" let g:VM_Extend_hl = 'Visual'
" let g:VM_Cursor_hl = 'Cursor'
" let g:VM_Insert_hl = 'Cursor'

" }}}
" Hydra				{{{

" HydraRed		#f2594b  #FF5733
" HydraBlue		#0091f7  #5EBCF6
" HydraAmaranth #FF355E  #ff1757
" HydraTeal		#009090  #00b5b5
" HydraPink		#f766ad  #ff55de

" #5EBCF6
" #419eff
" #4ba0ff

highlight HydraRed		  guifg=#FF5733 gui=bold
highlight HydraBlue		  guifg=#4ba0ff gui=bold
highlight HydraAmaranth   guifg=#ff1757 gui=bold
highlight HydraTeal		  guifg=#00b5b5 gui=bold
highlight HydraPink		  guifg=#ff57af gui=bold

" }}}
" Clever-f			{{{

" highlight CleverFDefaultLabel guibg=#6f7be5 guifg=#32302f gui=bold

" }}}

" vim: fdm=marker
