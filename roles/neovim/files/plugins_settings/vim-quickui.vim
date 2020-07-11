" set the color scheme
" let g:quickui_color_scheme = 'borland'
let g:quickui_color_scheme = 'gruvbox'
" let g:quickui_color_scheme = 'solarized'
" let g:quickui_color_scheme = 'papercol dark'

" enable to display tips in the cmdline
let g:quickui_show_tip = 1

let g:quickui_border_style = 2

" clear all the menus
call quickui#menu#reset()

" " install a 'File' menu, use [text, command] to represent an item.
" call quickui#menu#install('&File', [
"             \ [ "&New File\tCtrl+n", 'echo 0' ],
"             \ [ "&Open File\t(F3)", 'echo 1' ],
"             \ [ "&Close", 'echo 2' ],
"             \ [ "--", '' ],
"             \ [ "&Save\tCtrl+s", 'echo 3'],
"             \ [ "Save &As", 'echo 4' ],
"             \ [ "Save All", 'echo 5' ],
"             \ [ "--", '' ],
"             \ [ "E&xit\tAlt+x", 'echo 6' ],
"             \ ])

" call quickui#menu#install("&File", [
"            \ [ "LeaderF &File", 'Leaderf file', 'Open file with leaderf'],
"            \ [ "LeaderF &Mru", 'Leaderf mru --regexMode', 'Open recently accessed files'],
"            \ [ "LeaderF &Buffer", 'Leaderf buffer', 'List current buffers in leaderf'],
"            \ [ "--", ],
"            \ [ "J&unk File", 'JunkFile', ''],
"            \ ])

" call quickui#menu#install("&File", [
"           \ [ "--", ],
"           \ [ "E&xit", 'qa' ],
"           \ ])

" " items containing tips, tips will display in the cmdline
" call quickui#menu#install('&Edit', [
"             \ [ '&Copy', 'echo 1', 'help 1' ],
"             \ [ '&Paste', 'echo 2', 'help 2' ],
"             \ [ '&Find', 'echo 3', 'help 3' ],
"             \ ])

" script inside %{...} will be evaluated and expanded in the string
call quickui#menu#install("&Marks", [
          \ ['&Buffer Marks', 'SignatureListBufferMarks', 'Show location list with buffer marks'],
          \ ['&Global Marks', 'SignatureListGlobalMarks', 'List only the global marks used in all buffers in the location list'],
          \ ['&Marks show/hide', 'SignatureToggleSigns', 'Show column with marks signs'],
          \ ])

" call quickui#menu#install("&Build", [
"           \ ["File &Execute\tF5", 'AsyncTask file-run'],
"           \ ["File &Compile\tF9", 'AsyncTask file-build'],
"           \ ["File E&make\tF7", 'AsyncTask emake'],
"           \ ["File &Start\tF8", 'AsyncTask emake-exe'],
"           \ ['--', ''],
"           \ ["&Project Build\tShift+F9", 'AsyncTask project-build'],
"           \ ["Project &Run\tShift+F5", 'AsyncTask project-run'],
"           \ ["Project &Test\tShift+F6", 'AsyncTask project-test'],
"           \ ["Project &Init\tShift+F7", 'AsyncTask project-init'],
"           \ ['--', ''],
"           \ ["T&ask List\tCtrl+F10", 'call MenuHelp_TaskList()'],
"           \ ['E&dit Task', 'AsyncTask -e'],
"           \ ['Edit &Global Task', 'AsyncTask -E'],
"           \ ['&Stop Building', 'AsyncStop'],
"           \ ])


let g:quickui_headers = ['h','hpp']
" let g:quickui_sources = ['c','cpp']

call quickui#menu#install("&Build", [
            \ ["&Switch to " . "%{index(g:quickui_headers, expand('%:e')) >= 0? 'Source' : 'Header'}" . " file",
            \     'CocCommand clangd.switchSourceHeader'],
            \ ], '<auto>', 'c,cpp')

" call quickui#menu#install("&Git", [
"             \ ['&View Diff', 'call svnhelp#svn_diff("%")'],
"             \ ['&Show Log', 'call svnhelp#svn_log("%")'],
"             \ ['File &Add', 'call svnhelp#svn_add("%")'],
"             \ [ "--", ],
"             \ ])

" :GV to open commit browser
" You can pass git log options to the command, e.g. :GV -S foobar.
" :GV! will only list commits that affected the current file
" :GV or :GV? can be used in visual mode to track the changes in the selected lines.
call quickui#menu#install("&Git", [
          \ ['Open &Commit browser', 'GV'],
          \ ['&List commits that affected the current file', 'GV!'],
          \ ['&Fills the location list with the revisions of the current file', 'GV?' ],
          \ ])

" call quickui#menu#install('&Tools', [
"             \ ['Compare &History', 'call svnhelp#compare_ask_file()', ''],
"             \ ['&Compare Buffer', 'call svnhelp#compare_ask_buffer()', ''],
"             \ ['--',''],
"             \ ['List &Buffer', 'call quickui#tools#list_buffer("FileSwitch tabe")', ],
"             \ ['List &Function', 'call quickui#tools#list_function()', ],
"             \ ['Display &Messages', 'call quickui#tools#display_messages()', ],
"             \ ['--',''],
"             \ ["&DelimitMate %{get(b:, 'delimitMate_enabled', 0)? 'Disable':'Enable'}", 'DelimitMateSwitch'],
"             \ ['Read &URL', 'call menu#ReadUrl()', 'load content from url into current buffer'],
"             \ ['&Spell %{&spell? "Disable":"Enable"}', 'set spell!', 'Toggle spell check %{&spell? "off" : "on"}'],
"             \ ['&Profile Start', 'call MonitorInit()', ''],
"             \ ['Profile S&top', 'call MonitorExit()', ''],
"             \ ["Relati&ve number %{&relativenumber? 'OFF':'ON'}", 'set relativenumber!'],
"             \ ["Proxy &Enable", 'call MenuHelp_Proxy(1)', 'setup http_proxy/https_proxy/all_proxy'],
"             \ ["Proxy D&isable", 'call MenuHelp_Proxy(0)', 'clear http_proxy/https_proxy/all_proxy'],
"             \ ])

" script inside %{...} will be evaluated and expanded in the string
call quickui#menu#install("&Option", [
          \ ['Set &Spell %{&spell? "Off":"On"}', 'set spell!'],
          \ ['Set &Relativenumber %{&relativenumber? "Off":"On"}', 'set relativenumber!'],
          \ ['Set &Cursor Line %{&cursorline? "Off":"On"}', 'set cursorline!'],
          \ ['Set &Paste %{&paste? "Off":"On"}', 'set paste!'],
          \ ])
          \

" register HELP menu with weight 10000
call quickui#menu#install('H&elp', [
          \ ["&Cheatsheet", 'help index', ''],
          \ ['T&ips', 'help tips', ''],
          \ ['--',''],
          \ ["&Tutorial", 'help tutor', ''],
          \ ['&Quick Reference', 'help quickref', ''],
          \ ['&Summary', 'help summary', ''],
          \ ], 10000)

" hit space twice to open menu
" noremap <space><space> :call quickui#menu#open()<cr>
noremap <F9> :call quickui#menu#open()<cr>


"----------------------------------------------------------------------
" Context menu
"----------------------------------------------------------------------
let content = [
            \ ["&Rename\t<leader>rn",        'call CocAction("rename")'],
            \ ["Go to &Definition\tgd",      'call CocAction("jumpDefinition")'],
            \ ["Go to &Type-Definition\tgy", 'call CocAction("jumpTypeDefinition")'],
            \ ["Go to &Implementation\tgi",  'call CocAction("jumpImplementation")'],
            \ ["Show R&eferences\tgr",       'call CocAction("jumpReferences")'],
            \ ['-'],
            \ ["Do&cumentation", 'execute &keywordprg " " expand("<cword>")'],
            \ ]
            " \ ["Go to D&eclaration",          'call CocAction("jumpDeclaration")'],
            " \ ["&Documentation", 'execute &keywordprg." ".expand("<cword>")'],
            " \ ["&Documentation\t\\cm", 'echo 600'],

" set cursor to the last position
let opts = {'index': g:quickui#context#cursor}

noremap K :call quickui#context#open(content, opts)<Cr>
" nnoremap <silent>K :call quickui#tools#clever_context('k', g:context_menu_k, {})<cr>

" nnoremap <F3> :call quickui#tools#preview_tag('')<cr>
