"-------------------------------------------------------------------------------
"  Easymotion key bindings
"-------------------------------------------------------------------------------
map  ;  <Plug>(easymotion-prefix)
nmap s  <Plug>(easymotion-bd-f)
xmap s  <Plug>(easymotion-bd-f)
omap s  <Plug>(easymotion-bd-f)
nmap ;w <Plug>(easymotion-w)
nmap ;b <Plug>(easymotion-b)
nmap ;l <Plug>(easymotion-lineanywhere)

"-------------------------------------------------------------------------------
"  Settings
"-------------------------------------------------------------------------------
let g:EasyMotion_verbose = 1
                        " If 0 disable all the messages the plugin creates, such
                        " as "EasyMotion: Jumping to [l,c]" and "EasyMotion:
                        " Cancelled".

let g:EasyMotion_use_upper = 1
                        " Use uppercase target labels and type as a lower case.

let g:EasyMotion_smartcase = 1
                        " type `l` and match `l` & `L`

let g:EasyMotion_use_smartsign_us = 1
                        " Smartsign (type `3` and match `3` & `#`)

let g:EasyMotion_startofline = 0
                        " При перемещении по строкам курсор будет прыгать не в
                        " начало строки, а в туже колонку, что и был.
