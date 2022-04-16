"   ██           ██   ██                ██
"  ░░           ░░   ░██               ░░
"  ███  ██████   ██ ██████     ██    ██ ██ ██████████
" ░░██ ░██░░░██ ░██░░░██░     ░██   ░██░██░░██░░██░░██
"  ░██ ░██  ░██ ░██  ░██      ░░██ ░██ ░██ ░██ ░██ ░██
"  ░██ ░██  ░██ ░██  ░██       ░░████  ░██ ░██ ░██ ░██
"  ░██ ░██  ░██ ░██  ░░███  ██  ░░██   ░██ ███ ░██ ░██
"  ░░  ░░   ░░  ░░    ░░░  ░░    ░░    ░░ ░░░  ░░  ░░

" --------------------- Nvui GUI ------------------------
if exists('g:nvui')
  source ~/.config/nvim/ginit.vim
endif
" -------------------------------------------------------

" set verbose=1

" https://github.com/lewis6991/impatient.nvim
lua require('impatient')

" General Config                                                       {{{
" ========================================================================
" Список кодировок для автоматического их определения
set fileencodings=utf-8,cp1251

let mapleader = "\<Space>"  " set <leader> key
let maplocalleader = ','

set virtualedit=block

set mouse=a         " Enable mouse in all modes
set hidden          " Buffers can exist in the background without being in
                    " a window.

set number          " Показывать нумерацию строк
" set relativenumber
set noautochdir     " Set pwd as the dir of the active file.
                    " WARNING: If set this option breakes
                    " ahmedkhalf/project.nvim plugin.

set showcmd         " Show incomplete cmds down the bottom
set showmode        " Show current mode down the bottom
set laststatus=2    " всегда отображать статусную строку
set novisualbell    " Отключаем пищалки и моргалки
set autoread        " Reload files changed outside vim
set termguicolors   " 24 bit color support

set ttimeoutlen=50  " Время переключения между режимами (default 50)
set timeoutlen=1000 " Время в мс в течении которого Vim ждёт продолжения
                    " многосимвольной команды.

" set completeopt-=preview  " Not to show preview window on complection
set completeopt=menu,menuone,noselect

set title
set titlestring=%t  " tile

set exrc    " Allow vim search local configuratin files in project folders.
set secure  " Disallows the use of :autocmd, shell and write commands in
            " local exrc files.

syntax enable   " Включить подсветку синтаксиса
" }}}

" Folding                                                              {{{
" ========================================================================

set foldmethod=marker
" set foldmethod=expr     " use folding based on treesitter
set foldexpr=nvim_treesitter#foldexpr()

set foldcolumn=auto:2
set foldlevelstart=0    " 0: to always start editing with all folds closed
set foldnestmax=20      " maximum nested fold level
set foldminlines=4      " minimum lines required to create fold
set foldopen=block,hor,mark,jump,percent,quickfix,search,tag,undo
" set foldopen=all

" set fillchars=foldclose:,foldopen:   "  

" }}}

" Python support                                                       {{{
" ========================================================================
set pyxversion=3
if has('unix')
    let g:python3_host_prog = '/usr/bin/python3'

    " Use conda environment
    " ----------------------------------------------------------------
    " let g:python3_host_prog = '/opt/miniconda3/envs/nvim/bin/python'
    " let g:python_host_prog  = '/opt/miniconda3/envs/nvim/bin/python'
elseif has('win32')
    let g:python3_host_prog = 'C:/scoop/apps/miniconda3/current/python.exe'
    let g:python_host_prog  = 'C:/scoop/apps/miniconda2/current/envs/nvimpy2/python.exe'
endif

" }}}

" Clipboard                                                            {{{
" ========================================================================
if has('wsl')
    let g:clipboard = {
          \   'name': 'wslclipboard',
          \   'copy': {
          \      '+': '/mnt/c/tools/win32yank.exe -i --crlf',
          \      '*': '/mnt/c/tools/win32yank.exe -i --crlf',
          \    },
          \   'paste': {
          \      '+': '/mnt/c/tools/win32yank.exe -o --lf',
          \      '*': '/mnt/c/tools/win32yank.exe -o --lf',
          \   },
          \   'cache_enabled': 1,
          \ }
" elseif has('unix')
endif
" }}}

" Indentation                                                          {{{
" ========================================================================

set autoindent      " Use the current indentation when creating a new line
                    " in Insert mode, both through normal Enter or o/O.

 set smartindent    " does the right thing (mostly) in programs
 set cindent        " stricter rules for C programs

set smarttab        " “Умная” расстановка отступов.
set expandtab       " Заменять табуляцию пробелами.
set tabstop=4       " Количество пробелов в одном символе табуляции.
set softtabstop=-1  " Sunchronize tabstop with shiftwidth.
set shiftwidth=4    " Количество пробелов на которое будет сдвинута
                    " строка командами >> или <<.

" Show invisible symbols. Display tabs and trailing spaces visually
set list listchars=tab:\ \ ,trail:· ",eol:¶ ",eol:¤

" }}}

" Formating text                                                        {{{
" =========================================================================

set formatoptions=tcqj  " Default: tcqj
                        " The behavior of 'gw' command.

set joinspaces  " Put two spaces after period.
set linebreak   " Wrap lines at convenient points.

" Use Par Unix utility for 'gq' command
setglobal formatprg=par\ -w75\ g
" setglobal formatprg=par\ -w80\ g
" setglobal formatprg=par\ -w80\ g\ q
set nobomb

function! UpdateFormatprg()
   let &g:formatprg = substitute( &g:formatprg , '\d\+$', &textwidth , "" )
endfunction

augroup UpdateFormatprgGroup
  autocmd!
  autocmd VimEnter,BufEnter * call UpdateFormatprg()
  autocmd OptionSet textwidth call UpdateFormatprg()
augroup END

" }}}

" =================== Completion ======================

" set wildmode=list:longest
                    " При автодополнении в командном режиме (:command) в
                    " начале списка показывать самый длинный вариант

set wildcharm=<C-z> " Символ, который активирует автодополнение в скриптовых
                    " коммандах или при назначении клавиш. Использование
                    " <Tab> в этих ситуациях непосредственно вставит символ
                    " табуляции, а не откроет меню дополнения.

set wildoptions=pum,tagfile
                    " Отключение опции 'pum' включает автодополнение в
                    " командной строке в старом стиле: в строку, а не в
                    " формате вертикального выпадающего списка.

" set wildoptions=tagfile

" =================== Pop-Up Menu ======================

set pumblend=0      " Enables pseudo-transparency for the popup-menu.
                    " With 0 value the transparency is disabled, which is
                    " necessary for proper work of lspkind-icons plugin.
                    " Because most Nerd Font icons require 2 cells where
                    " second shold be the space for correct render.  And
                    " with pseudo transparency turned on, the underlying
                    " characters can occupy a cell required for the space
                    " character.

" set pumheight=15    " Количество строк во всплывающем окне

" ==================== Scrolling =======================

set scrolloff=0     " Start scrolling when we're n lines away from margins.
setlocal scrolloff=0  " https://github.com/karb94/neoscroll.nvim/issues/28

set sidescrolloff=4 " Сколько колонок должно остаться до конца экрана,
                    " чтобы Vim начал прокручивать экран вбок.
set sidescroll=1    "   Минимальное количество колонок на которое экран
                    " будет прокручиваться горизонтально за раз.

" ===================== Search =========================

set incsearch       " Подсвечивать найденный текст по мере набора
set hlsearch        " Highlight searches by default
set ignorecase      " Ignore case when searching...
set smartcase       " ...unless we type a capital

set inccommand=split " Shows a preview window of all the changes you are
                     " going to make in the document.

" ===================== Spelling =======================

set spelllang=ru_ru,en_us
" set dictionary      " используемые словари

" ====================== Syntax ========================

" https://github.com/tpope/vim-markdown
let g:markdown_fenced_languages = [
    \ 'lua', 'vim', 'shell=sh', 'bash=sh', 'cpp', 'python', 'json', 'html'
    \ ]
let g:markdown_minlines = 100   " Syntax highlight is synchronized in 100 lines.

" let g:markdown_folding = 1
                    " Enable folding in markdown files. The value of the
                    " variable does not matter. It just should be set.
                    " :help ft-markdown-plugin

let g:vimsyn_embed = 'lPr'  " Turn on syntax highlighting for embeded lua,
                            " python and ruby pieces of code inside
                            " Vimscript in *.vim files.

" =============== Turn Off Swap Files ==================

set noswapfile

" Needed for coc.nvim: some LSP servers have issues with
" backup files, see #649.
set nobackup
set nowritebackup

" =================== Русский язык =====================

set keymap=russian-jcukenwin
set iminsert=0  "   Чтобы при старте ввод был на английском,
                " а не русском (start > i).
set imsearch=0  "   Чтобы при старте поиск был на английском,
                " а не русском (start > /).

" " Менять цвет курсора при включенном русском языке
" highlight Cursor guifg=Cyan guibg=Green ctermbg=Blue ctermfg=Cyan
" highlight lCursor guifg=NONE guibg=green

" highlight Cursor guifg=NONE guibg=#e8ae3c
" highlight lCursor guifg=NONE guibg=#e7ae3c

set helplang=ru     " Помощь на русском языке

" ================== Visual Tweaks =====================

set signcolumn=auto:1
set nowrap          " Wrap lines
set cmdheight=1     " Make command line one line high.
set colorcolumn=+1  " Показывать рулетку в следующей колонке после textwidth.
set mousehide       " Hide the mouse when typing text.
" set cursorline      " Выделять строку, на которой находится курсор.

" set guicursor=a:blinkon100  " Turn on cursor blinking

" hi Cursor guifg=NONE guibg=cyan
" hi Cursor2 guifg=NONE guibg=red

" Change cursor shape between modes
set guicursor=n-v-c:block-Cursor/lCursor
set guicursor+=i-ci-ve:ver25-Cursor2/lCursor2,r-cr:hor20,o:hor50

" ================== Window Splits =====================

" Open splits on the left and above
set splitbelow
set splitright

"                Settings that unnecessary in Neovim                   {{{
" ------------------------------------------------------------------------

" set encoding=utf-8
" set backspace=indent,eol,start
"
" set history=10000 " Store lots of :cmdline history.
"                   " Defaults: 10000 (the maximum)
"
" set wildmenu      " Enable ctrl-n and ctrl-p to scroll through matches.
"
" filetype plugin indent on
"       " Is a short form of these commands:
"       "
"       "     filetype on
"       "     filetype plugin on
"       "     filetype indent on
"       "
"       " The first command turns on filetype detection for Vim to help
"       " set syntax highlighting and other options. The plugin part will
"       " load plugins for specific filetype if they exist. The last bit
"       " will load indent file for specific filetype if they exist too.
"       "
"       " For example, if you want to activate certain plugins for only
"       " Python language, then you can create a file
"       " ~/.vim/ftplugin/python.vim. Put all the plugins and commands you
"       " want specifically for Python inside that file.
"       "
"       " A good practice is to separate the indent configuration inside
"       " another file (~/.vim/indent/python.vim).  However, I usually
"       " just put the indents inside the plugin file.

" ---------------------------------------------------------------------}}}

"                     Autocommands and Functions                       {{{
" ========================================================================

" https://www.bobbywlindsey.com/2017/07/30/vim-functions/
" convert rows of numbers or text (as if pasted from excel column) to a tuple
command! -range ToTuple <line1>,<line2> call ToTupleFunction()
function! ToTupleFunction() range "{{{
    silent execute a:firstline . "," . a:lastline . "s/^/'/"
    silent execute a:firstline . "," . a:lastline . "s/$/',/"
    silent execute a:firstline . "," . a:lastline . "join"
    silent execute "normal I("
    silent execute "normal $xa)"
    silent execute "normal ggVGYY"
endfunction "}}}


" " Automatically deletes all trailing whitespace on save
" " Автоматически удалять все лишние пробелы в конце строк при сохранении
" " Replaced with the 'vim-better-whitespace' plugin
" autocmd BufWritePre * %s/\s\+$//e


" " Delete all trailing blank lines at the end of the file on save
" autocmd BufWritePre * call TrimEndLines()
" function! TrimEndLines()
"     let save_cursor = getpos(".")
"     :silent! %s#\($\n\s*\)\+\%$##
"     call setpos('.', save_cursor)
" endfunction


" " Reloads vimrc after saving but keep cursor position
" if !exists('*ReloadVimrc')
"    fun! ReloadVimrc()
"        let save_cursor = getcurpos()
"        source $MYVIMRC
"        call setpos('.', save_cursor)
"        execute 'AirlineRefresh'
"        execute 'RainbowToggleOn'
"    endfun
" endif
" autocmd! BufWritePost $MYVIMRC call ReloadVimrc()

" autocmd FileType man set bufhidden=unload
" autocmd FileType man if (winnr("$") != 1) | wincmd H | endif


" Automatically resize splits when resizing window
" autocmd VimResized * wincmd =

" autocmd BufRead,BufNewFile *.txt setlocal textwidth=78

" Open Startify buffer in new tab page
" autocmd BufEnter * if !exists('t:startified') | Startify | let t:startified = 1 | endif

" Open preview window in vertical split
" :autocmd WinEnter * if &previewwindow && winnr() > 1 | wincmd K | endif
" autocmd WinEnter * if &previewwindow | wincmd L | endif


" Tmux windows names
" autocmd BufReadPost,FileReadPost,BufNewFile * call system("tmux rename-window " . expand("%"))
" autocmd BufEnter * let &titlestring = ' ' . expand("%:t")
" set title

" Highlight on yank. For explanations see `:help lua-highlight`
au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=250}

"}}}

"                            Key Bindings                              {{{
" ========================================================================
" Синтаксис создания комбинации
" [[mode]nore]map keys command
" где mode — режим или окружение, где работает комбинация
" nore (non-recursive) — не «раскрывать» комбинацию,
" а использовать значение по умолчанию
" ========================================================================

lua require('keybindings')

" Fix writing :W to save
command! W w

" nnoremap <expr> zz "zz" . (winheight(0) * 0.1)->float2nr() . "<c-e>"

" Копировать, вставить
" vnoremap <silent> <C-c> "+y <cmd>let @*=@+<CR>
nnoremap yc    "+y
nnoremap ycc   "+yy
nnoremap <C-c> "+y
nnoremap <C-c><C-c> "+yy
vnoremap <C-c> "+y
nnoremap cp    "+p
nnoremap cP    "+P
vnoremap <C-p> "+p


" Перемещаться в начало / конец строки по 'Shift + h/l'
noremap H ^
noremap L $

" Отключить подсветку только что найденного текста
nnoremap <silent> <Esc> <cmd>nohlsearch<CR><Esc>


" При нажатии <Enter> вставлять пустую сроку снизу если буфер имеет
" свойство 'изменяемый' и окно в котором он открыт не является 'command
" line window'.  В противном случае трактовать <Enter> как обычно. Это
" сохраняет стандартное поведение <Enter> в quickfix menu типо
" оглавлений.
" ( if ?  then : else )
" nnoremap <expr> <Enter> &modifiable && getcmdwintype() == '' ? "o\<Esc>" : "\<CR>"


" Change the behavior of the <Enter> key when the popup menu is visible.
" The <Enter> key will select the highlighted menu item, just as <C-Y>
" does.
" <C-g>u : break undo sequence, start new change (:help i_CTRL-G_u)
inoremap <expr> <Enter> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"


" часто нужно отделять блоки кода пустой строкой, например, перед return
nnoremap 2o o<CR>
nnoremap 2O O<Esc>O

" WARNING: Replaced with 'christoomey/vim-tmux-navigator' plugin
" Quick jumping between splits
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-H> <C-W>h
map <C-L> <C-W>l

nnoremap <A-Up> <C-W>+
nnoremap <A-Down> <C-W>-
nnoremap <A-Left> <C-W><
nnoremap <A-RIght> <C-W>>

" Open new splits easily
map vv <C-W>v
" map ss <C-W>s

" Close preview window if exists, else close current window
map <silent> Q <cmd>call CloseWindow()<CR>
function! CloseWindow() "{{{
    if bufexists("[Command Line]")
        close  " Close command line window like q:, q/, q? etc.
    elseif IsPreviewWindowOpen()
        pclose  " Close preview window
    elseif IsFileTypeOpen('qf')
        " Close quickfix window specified by its number, returned by
        "   IsFileTypeOpen() function.
        execute IsFileTypeOpen('qf').'close'
    elseif IsFileTypeOpen('nerdtree')
        " NerdTree plugim
        NERDTreeClose
    elseif IsFileTypeOpen('man')
        execute IsFileTypeOpen('man').'close'
    elseif IsFileTypeOpen('help')
        helpclose  " Close help window.
        doautocmd User MyCustomCloseWindow
        " doautocmd WinEnter
    else
        close  " Close the current window, except if it the last.
    endif
endfunction "}}}
function! IsPreviewWindowOpen()  "{{{
    " Check if 'preview window' is open.  Return 1 or 0.

    for window in range(1, winnr('$'))
        " getwinvar(номер_окна, переменная) - получить значение
        " переменной заданного окна.
        if getwinvar(window, '&previewwindow') == 1
            " found a preview window
            return 1
        endif
    endfor
    return 0
endfunction
" }}}
function! IsFileTypeOpen(name)  "{{{
    " Signature: string -> number
    " Accepts string with filetype (vim, py, cpp, ...) or 'quickfix'
    " special word and returns the number of first window in current tab
    " according to vim rules that contains buffer with this filetype or
    " quickfix window. If there is no such window returns 0.

    " If argument of the function is 'qf' or 'quickfix'
    if index(['qf', 'quickfix'], a:name) >= 0
        for window in range(1, winnr('$'))
            if getbufvar(winbufnr(window), '&buftype') == 'quickfix'
                return window
            endif
        endfor
    else
        for window in range(1, winnr('$'))
            " Get window variable
            if getwinvar(window, '&filetype') == a:name
                return window
            endif
        endfor
    endif
    return 0
endfunction  "}}}

" In insert mode, move normally by using Ctrl
cnoremap <C-h> <Left>
" cnoremap <C-j> <Down>
" inoremap <C-k> <Up>
inoremap <C-l> <Right>

" In command mode, move normally by using Ctrl
cnoremap <C-h> <BS>
cnoremap <C-j> <Down>
cnoremap <C-k> <Up>
cnoremap <C-l> <Right>

" " Save changes in buffer
" nmap <silent> <Leader>s :write<Enter>

" открыть файл конфигурации .vimrc или init.vim
command! I edit $MYVIMRC
" открыть файл с плагинами
if has('unix')
    command! P edit ~/.config/nvim/plugin/plugins.lua
    " command! P edit ~/.config/nvim/lua/plugins.lua
endif

" Включать и выклюяать проверку орфографии
map <silent> <F6> :call ToggleSpell()<CR>
fu! ToggleSpell() "{{{
    " Toggle the spell state
    setlocal spell!
    " echo &spell ? 'spell' : 'nospell'
    echo &spell ? 'spell on' : 'spell off'
endf "}}}


" " Переключать язык по нажатию Ctrl-\ вместо Ctrl-^, как в Emacs.
" " Switch keyboard layout (language) on Ctrl-\ instead of Ctrl-^,
" " like in Emacs.
" inoremap <silent> <C-\> <C-O>:call ToggleKeyboardLayout()<CR>
" nnoremap <silent> <C-\> :call ToggleKeyboardLayout()<CR>

" " Switch keyboard layout (language) on Ctrl-Space instead of Ctrl-^.
" " ----------------------------------------------------------
" inoremap <silent> <C-Space> <C-O>:call ToggleKeyboardLayout()<CR>
" nnoremap <silent> <C-Space> :call ToggleKeyboardLayout()<CR>
" fu! ToggleKeyboardLayout() "{{{
"     if &iminsert == 0
"         " set iminsert=1
"         setlocal iminsert=1
"     elseif &iminsert == 1
"         " set iminsert=0
"         setlocal iminsert=0
"     endif
" endf "}}}

" }}}

" set winwidth=80

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

" vim: fdm=marker number tw=76 cc=+1
