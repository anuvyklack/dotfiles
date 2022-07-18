"   ██           ██   ██                ██
"  ░░           ░░   ░██               ░░
"  ███  ██████   ██ ██████     ██    ██ ██ ██████████
" ░░██ ░██░░░██ ░██░░░██░     ░██   ░██░██░░██░░██░░██
"  ░██ ░██  ░██ ░██  ░██      ░░██ ░██ ░██ ░██ ░██ ░██
"  ░██ ░██  ░██ ░██  ░██       ░░████  ░██ ░██ ░██ ░██
"  ░██ ░██  ░██ ░██  ░░███  ██  ░░██   ░██ ███ ░██ ░██
"  ░░  ░░   ░░  ░░    ░░░  ░░    ░░    ░░ ░░░  ░░  ░░

lua << EOF
if not pcall(require, 'impatient') then
   print 'Failed to load impatient'
end
EOF

" -- Nvui GUI
if exists('g:nvui') | source ~/.config/nvim/ginit.vim | endif

" == Options ============================================================= {{{

set fileencodings=utf-8,cp1251
set termguicolors	" 24 bit color support

let mapleader = "\<Space>"
let maplocalleader = ','

set virtualedit=block
set undofile		" Save undo history in a file between sessions
set mouse=a			" Enable mouse in all modes
set splitbelow		" Open splits below and ...
set splitright		" ... to the right.

" set number
" set relativenumber

" set autochdir		" Set pwd as the dir of the active file.
" 					" WARNING: Seting this option breakes
" 					" `ahmedkhalf/project.nvim' plugin.

set updatetime=1000	" The time until the CursorHold autocommand will triggered.

set title titlestring=%t	" tile

" set iskeyword+=^_	" Consider underscore as word separator (for w, dw, ...).
" set iskeyword-=_	" Consider underscore as word separator (for w, dw, ...).

set exrc			" Allow vim search local configuratin files in project folders.
set secure			" Disallows the use of :autocmd, shell and write commands
					" in local exrc files.
" syntax enable

" == Turn Off Swap Files =====================================================
set noswapfile

" Needed for coc.nvim: some LSP servers have issues with backup files,
" see: https://github.com/neoclide/coc.nvim/issues/649
set nobackup
set nowritebackup

" == Formating text ==========================================================
set textwidth=80
set formatoptions+=21	" The behavior of 'gw' command.
set joinspaces			" Put two spaces after period.
set linebreak			" Wrap lines at convenient points.
set nobomb

" Use Par Unix utility for 'gq' command
" setglobal formatprg=par\ -w75\ g
" setglobal formatprg=par\ -w80\ g
" setglobal formatprg=par\ -w80\ g\ q

" function! UpdateFormatprg()
"    let &g:formatprg = substitute( &g:formatprg , '\d\+$', &textwidth , "" )
" endfunction
"
" augroup UpdateFormatprgGroup
"   autocmd!
"   autocmd VimEnter,BufEnter * call UpdateFormatprg()
"   autocmd OptionSet textwidth call UpdateFormatprg()
" augroup END

" == Folds ===================================================================
set foldcolumn=auto:2
set foldlevel=0
set foldlevelstart=0	" 0: to always start editing with all folds closed

" set foldnestmax=20	" maximum nested fold level
" set foldminlines=4	" minimum lines required to create fold
set foldopen+=jump

" set foldmethod=marker
" set foldmethod=expr	" use folding based on treesitter by default
set foldexpr=nvim_treesitter#foldexpr()

" set fillchars=foldclose:,foldopen:	 "  

" == Indent ==================================================================
set smartindent
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4

set smartindent			" does the right thing (mostly) in programs
" set cindent			" stricter rules for C programs

set smarttab			" “Умная” расстановка отступов.
set expandtab			" Заменять табуляцию пробелами.
set tabstop=4			" Количество пробелов в одном символе табуляции.
set softtabstop=-1		" Synchronize tabstop with shiftwidth.
set shiftwidth=4		" Количество пробелов на которое будет сдвинута
						" строка командами >> или <<.

" Show invisible symbols. Display tabs and trailing spaces visually
set list
set listchars=tab:›\ ,trail:‧,nbsp:+
" set listchars=tab:⇥\ ,trail:‧,nbsp:+
" set listchars+=eol:¤
" set listchars+=eol:¶

" == Visual Tweaks ===========================================================
set laststatus=3		" Use global statusline
set signcolumn=auto:1
set nowrap				" Wrap lines
" set cmdheight=1			" Make command line one line high.
set colorcolumn=+1		" Показывать рулетку в следующей колонке после textwidth.
set mousehide			" Hide the mouse when typing text.
" set cursorline		" Выделять строку, на которой находится курсор.
set cursorlineopt=number,screenline

" " Change cursor shape between modes
" hi Cursor guifg=green guibg=green
" hi Cursor2 guifg=red guibg=red
" set guicursor=n-v-c:block-Cursor/lCursor,i-ci-ve:ver25-Cursor2/lCursor2,r-cr:hor20,o:hor50
" " set guicursor=i-ci-ve:ver25-Cursor2/lCursor2,r-cr:hor20,o:hor50

" == Search ==================================================================
set ignorecase			" Ignore case when searching...
set smartcase			" ...unless we type a capital.
set inccommand=split	" Shows a preview window of all the changes you are
						" going to make in the document.

" == Completion ==============================================================
set completeopt=menu,menuone,noselect
" set completeopt-=preview  " Not to show preview window on complection

" set wildmode=list:longest
						" При автодополнении в командном режиме (:command)
						" в начале списка показывать самый длинный вариант

set wildcharm=<C-z>		" Символ, который активирует автодополнение
						" в скриптовых коммандах или при назначении клавиш.
						" Использование <Tab> в этих ситуациях непосредственно
						" вставит символ табуляции, а не откроет меню
						" дополнения.

set wildoptions=pum,tagfile
						" Отключение опции 'pum' включает автодополнение
						" в командной строке в старом стиле: в строку, а не
						" в формате вертикального выпадающего списка.

" set wildoptions=tagfile

" == Pop-Up Menu =============================================================

" set pumblend=0		" Enables pseudo-transparency for the popup-menu.
						" With 0 value the transparency is disabled, which is
						" necessary for proper work of lspkind-icons plugin.
						" Because most Nerd Font icons require 2 cells where
						" second shold be the space for correct render.  And
						" with pseudo transparency turned on, the underlying
						" characters can occupy a cell required for the space
						" character.

" set pumheight=15		" Количество строк во всплывающем окне

" == Scrolling ===============================================================
set scrolloff=0			" Start scrolling when we're n lines away from margins.
setlocal scrolloff=0	" https://github.com/karb94/neoscroll.nvim/issues/28

set sidescrolloff=0		" Сколько колонок должно остаться до конца экрана,
						" чтобы Vim начал прокручивать экран вбок.

set sidescroll=1		" Минимальное количество колонок на которое экран
						" будет прокручиваться горизонтально за раз.

" == Spelling ================================================================
set spelllang=ru_ru,en_us
" set dictionary		" using dictionaries

" == Русский язык ======================================================== {{{
set keymap=russian-jcukenwin
set iminsert=0	" Чтобы при старте ввод был на английском, а не русском (start > i).
set imsearch=-1	" Чтобы при старте поиск был на английском, а не русском (start > /).

" " Менять цвет курсора при включенном русском языке
" highlight Cursor guifg=Cyan guibg=Green ctermbg=Blue ctermfg=Cyan
" highlight lCursor guifg=NONE guibg=green

" highlight Cursor guifg=NONE guibg=#e8ae3c
" highlight lCursor guifg=NONE guibg=#e7ae3c

set helplang=ru     " Помощь на русском языке
" }}}

" == Markdown ============================================================ {{{
" https://github.com/tpope/vim-markdown
let g:markdown_fenced_languages = [
\	'lua', 'vim', 'shell=sh', 'bash=sh', 'cpp', 'python', 'json', 'html'
\ ]
let g:markdown_minlines = 100	" Syntax highlight is synchronized in 100 lines.

let g:markdown_folding = 1
					" Enable folding in markdown files. The value of the
					" variable does not matter. It just should be set.
					" :help ft-markdown-plugin

let g:vimsyn_embed = 'lPr'
					" Turn on syntax highlighting for embeded lua, python
					" and ruby pieces of code inside Vimscript in *.vim files.
" }}}

" == Clipboard =========================================================== {{{
if has('wsl')
	let g:clipboard = {
	\	'name': 'wslclipboard',
	\	'copy': {
	\	   '+': '/mnt/c/tools/win32yank.exe -i --crlf',
	\	   '*': '/mnt/c/tools/win32yank.exe -i --crlf',
	\	 },
	\	'paste': {
	\	   '+': '/mnt/c/tools/win32yank.exe -o --lf',
	\	   '*': '/mnt/c/tools/win32yank.exe -o --lf',
	\	},
	\	'cache_enabled': 1,
	\ }
" elseif has('unix')
endif " }}}

" == Python support ====================================================== {{{
if has('unix')
	let g:python3_host_prog = '/usr/bin/python3'

	" Use conda environment
	" ----------------------------------------------------------------
	" let g:python3_host_prog = '/opt/miniconda3/envs/nvim/bin/python'
	" let g:python_host_prog  = '/opt/miniconda3/envs/nvim/bin/python'
elseif has('win32')
	let g:python3_host_prog = 'C:/scoop/apps/miniconda3/current/python.exe'
	let g:python_host_prog	= 'C:/scoop/apps/miniconda2/current/envs/nvimpy2/python.exe'
endif
" }}}

" == Settings that unnecessary in Neovim ================================= {{{

" set encoding=utf-8
" set backspace=indent,eol,start
" set showcmd			" Show incomplete cmds down the bottom.
" set showmode			" Show current mode down the bottom.
" set novisualbell		" Отключаем пищалки и моргалки.
" set autoread			" Reload files changed outside vim.
" set ttimeoutlen=50	" Время переключения между режимами (default 50)
" set timeoutlen=1000	" Время в мс в течении которого Vim ждёт продолжения
"						" многосимвольной команды.
" set history=10000		" Store lots of :cmdline history. Defaults: 10000 (the maximum)
" set hidden			" Buffers can exist in the background without being in
"						" a window.
" set wildmenu			" Enable ctrl-n and ctrl-p to scroll through matches.
" set incsearch			" Подсвечивать найденный текст по мере набора.
" set hlsearch			" Highlight searches by default.

" filetype plugin indent on
"		" Is a short form of these commands:
"		"
"		"	  filetype on
"		"	  filetype plugin on
"		"	  filetype indent on
"		"
"		" The first command turns on filetype detection for Vim to help set
"		" syntax highlighting and other options. The plugin part will load
"		" plugins for specific filetype if they exist. The last bit will load
"		" indent file for specific filetype if they exist too.
"		"
"		" For example, if you want to activate certain plugins for only Python
"		" language, then you can create a file ~/.vim/ftplugin/python.vim.
"		" Put all the plugins and commands you want specifically for Python
"		" inside that file.
"		"
"		" A good practice is to separate the indent configuration inside
"		" another file (~/.vim/indent/python.vim).	However, I usually just
"		" put the indents inside the plugin file.
"
" set autoindent		" Use the current indentation when creating a new line
"						" in Insert mode, both through normal Enter or o/O.
" set pyxversion=3

" }}}

" }}}

" == Diagnostic ========================================================== {{{
"       󰂭
"     
"  
"  	
"  	
"    
call sign_define("DiagnosticSignError", {"text": "", "texthl": "DiagnosticSignError"})
call sign_define("DiagnosticSignWarn",	{"text": "", "texthl": "DiagnosticSignWarn" })
call sign_define("DiagnosticSignInfo",	{"text": "", "texthl": "DiagnosticSignInfo" })
call sign_define("DiagnosticSignHint",	{"text": "", "texthl": "DiagnosticSignHint" })
" }}}

" == Plugins ============================================================= {{{

let packer_path = stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if empty(glob(packer_path)) > 0
	call system(['git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', packer_path])
	packadd packer.nvim
	lua require('plugins').sync()
endif

command! Packer	       packadd packer.nvim | lua require('plugins')
command! PackerInstall packadd packer.nvim | lua require('plugins').install()
command! PackerUpdate  packadd packer.nvim | lua require('plugins').update()
command! PackerSync    packadd packer.nvim | lua require('plugins').sync()
command! PackerClean   packadd packer.nvim | lua require('plugins').clean()
command! PackerCompile packadd packer.nvim | lua require('plugins').compile()

" }}}

" == Functions, Commands, Autocommands =================================== {{{

" Open init.vim and plugins.lua files.
command! I edit $MYVIMRC
command! P execute 'edit '..stdpath('config')..'/lua/plugins.lua'

lua P = vim.pretty_print
lua prequire = require('util').prequire

" Execute command preserve screen and cursor positions, search register and
" jumps list.
function! PreserveScreen(command) " {{{
	" let .............. used to set a variable
	" l:somevar ........ local variable
	" winsaveview() .... get information about window view
	" winrestview(view)  restores window view to its last status
	" getreg('/') ...... used to store the last search in a variable
	" keepjumps ........ used to performe any change without change jumplis
	try
		" Preparation: save last search, and cursor position.
		let l:win_view = winsaveview()
		let l:old_query = getreg('/')
		silent! execute 'keepjumps '..a:command
	finally
		" try restore / reg and cursor position
		call winrestview(l:win_view)
		call setreg('/', l:old_query)
	endtry
endfunction " }}}

" Highlight on yank, see ':help lua-highlight'
au TextYankPost * silent! lua vim.highlight.on_yank{ higroup="IncSearch", timeout=250 }

augroup HelpSplit
	autocmd!
	autocmd WinNew * autocmd BufEnter * ++once call <SID>HelpSplit()
aug end
function! <SID>HelpSplit() " {{{
	if (&bt ==? 'help' || &ft ==? 'man' || &ft ==? 'fugitive' || &ft ==? 'gitcommit')
		let p = winnr('#')
		" if winwidth(p) >= getwinvar(p, '&tw', 80) + 80
		if winwidth(p) >= getwinvar(p, '&tw', 80) + 20
			let b = bufnr()
			let bh = &l:bufhidden
			setlocal bufhidden=hide
			wincmd p
			execute winnr('#')..'wincmd q'
			vsplit
			execute b..'buffer'
			let &l:bufhidden = bh
		endif
	endif
endfunction " }}}

" https://www.bobbywlindsey.com/2017/07/30/vim-functions/
" Convert rows of numbers or text (as if pasted from excel column) to a tuple
command! -range ToTuple <line1>,<line2> call ToTupleFunction()
function! ToTupleFunction() range "{{{
    silent execute a:firstline . "," . a:lastline . "s/^/'/"
    silent execute a:firstline . "," . a:lastline . "s/$/',/"
    silent execute a:firstline . "," . a:lastline . "join"
    silent execute "normal I("
    silent execute "normal $xa)"
    silent execute "normal ggVGYY"
endfunction "}}}

" autocmd FileType man set bufhidden=unload
" autocmd FileType man if (winnr("$") != 1) | wincmd H | endif

" Unused {{{

" " Включать и выключать проверку орфографии
" map <silent> <F6> <Cmd>call ToggleSpell()<CR>
" function! ToggleSpell() "{{{
"     setlocal spell! " toggle the spell state
"     echo &spell ? 'spell on' : 'spell off'
" endfunction "}}}

" " Auto adjust width of the active window.
" set winwidth=80
"
" set noequalalways
" augroup ReduceNoise
"     autocmd!
"     " Automatically resize active split to 85 width
"     autocmd WinEnter * :call ResizeSplits()
" augroup END
"
" function! ResizeSplits()
"     set winwidth=80
"     wincmd =
" endfunction


" " Включать и выклюяать проверку орфографии
" map <silent> <F6> <Cmd>call ToggleSpell()<CR>
" function! ToggleSpell() "{{{
" 	setlocal spell! " toggle the spell state
" 	echo &spell ? 'spell on' : 'spell off'
" endfunction "}}}


" " Delete all trailing blank lines at the end of the file on save
" autocmd BufWritePre * call TrimEndLines()
" function! TrimEndLines()
"     let save_cursor = getpos(".")
"     :silent! %s#\($\n\s*\)\+\%$##
"     call setpos('.', save_cursor)
" endfunction


" Tmux windows names
" autocmd BufReadPost,FileReadPost,BufNewFile * call system("tmux rename-window " . expand("%"))
" autocmd BufEnter * let &titlestring = ' ' . expand("%:t")
" set title


" Open preview window in vertical split
" :autocmd WinEnter * if &previewwindow && winnr() > 1 | wincmd K | endif
" autocmd WinEnter * if &previewwindow | wincmd L | endif

" }}}

" }}}

" vim: fdm=marker cc=+1 ts=4 sts=4
