" ----------------------- Folds -------------------------

" hi Folded guifg=#DDAE52
" hi Folded guifg=#e6b24c
" hi Folded guifg=#e8ae3c
" hi Folded guifg=#ECA825

hi Folded guifg=#e3a84e guibg=None

hi FoldColumn guifg=#e8ae3c

hi link NormalFloat Pmenu

" -------------------- Easymotion -----------------------

highlight EasyMotionTarget guifg=#ff1414 gui=bold
highlight EasyMotionTarget2First guifg=#ffb912 gui=bold
highlight EasyMotionTarget2Second guifg=#cf9200
" highlight! link EasyMotionShade Grey


" EasyMotion defaults:
" -------------------
" highlight EasyMotionTarget guifg=#ff0000 gui=bold
" highlight EasyMotionTarget2First guifg=#ffb400 gui=bold
" highlight EasyMotionTarget2Second guifg=#b98300

" ----------------- Hop (Easymotion) --------------------

highlight HopNextKey   guifg=#ff007c gui=bold
highlight HopNextKey1  guifg=#00b7d1 gui=bold
" highlight HopNextKey1  guifg=#0095a8 gui=bold
highlight HopNextKey2  guifg=#00a3ba
highlight HopUnmatched guifg=#aaaaaa


" ------------------ Color brackets ---------------------

" 'fg0':              ['#e2cca9',   '223'],
" 'fg1':              ['#e2cca9',   '223'],
" 'red':              ['#f2594b',   '167'],
" 'orange':           ['#f28534',   '208'],
" 'yellow':           ['#e9b143',   '214'],
" 'green':            ['#b0b846',   '142'],
" 'aqua':             ['#8bba7f',   '108'],
" 'blue':             ['#80aa9e',   '109'],
" 'purple':           ['#d3869b',   '175'],
" 'bg_red':           ['#db4740',   '167'],
" 'bg_green':         ['#b0b846',   '142'],
" 'bg_yellow':        ['#e9b143',   '214']

" {'guifgs': ['#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'] }
" {'guifgs': ['#f28534', '#f2594b', '#80aa9e', '#e9b143', '#b0b846', '#d3869b'] }


lua << EOF
require'nvim-treesitter.configs'.setup{
  rainbow = {
    colors = {
      '#f28534', '#80aa9e', '#e9b143', '#b0b846', '#d3869b', '#f2594b'
    },
  },
}
EOF


" vim: ts=2 sts=2 sw=2
