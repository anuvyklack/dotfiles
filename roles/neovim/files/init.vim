"   ██           ██   ██                ██
"  ░░           ░░   ░██               ░░
"  ███  ██████   ██ ██████     ██    ██ ██ ██████████
" ░░██ ░██░░░██ ░██░░░██░     ░██   ░██░██░░██░░██░░██
"  ░██ ░██  ░██ ░██  ░██      ░░██ ░██ ░██ ░██ ░██ ░██
"  ░██ ░██  ░██ ░██  ░██       ░░████  ░██ ░██ ░██ ░██
"  ░██ ░██  ░██ ░██  ░░███  ██  ░░██   ░██ ███ ░██ ░██
"  ░░  ░░   ░░  ░░    ░░░  ░░    ░░    ░░ ░░░  ░░  ░░

" Vim special settings {{{
if !has('nvim')
    set nocompatible  " This must be first, because it changes other
                      " options as a side effect.
    set t_Co=256
endif
" }}}

" General Config                                                     {{{
" ======================================================================
" Список кодировок для автоматического их определения
set fileencodings=utf-8,cp1251

let mapleader = "\<Space>"      " клавиша, соответствующая ключю <leader>
let maplocalleader = ','

" " If disabled on Ubuntu "spellfile.vim" plugin unable to download spell
" " files. With Debian Neovim, though, it works.
" let loaded_netrwPlugin = 1 " If 0 disabled netrw

" http://items.sjbach.com/319/configuring-vim-right
set mouse=a                     " Enable mouse in all modes
set hidden                      " buffers can exist in the background
                                "   without being in a window.
set number                      " Показывать нумерацию строк
" set relativenumber
set backspace=indent,eol,start  " Allow backspace in insert mode
set history=1500                " Store lots of :cmdline history
set showcmd                     " Show incomplete cmds down the bottom
set showmode                    " Show current mode down the bottom
set laststatus=2                " всегда отображать статусную строку
set novisualbell                " Отключаем пищалки и моргалки
set autoread                    " Reload files changed outside vim
set autochdir                   " set pwd as the dir of the active file
set termguicolors               " 24 bit color support
set encoding=utf-8
set timeout timeoutlen=1500     " Время в мс в течении которого Vim ждёт
                                "   продолжения многосимвольной команды.
set ttimeoutlen=50              " Время переключения между режимами (default 50)
syntax enable                   " Включить подсветку синтаксиса

" set completeopt-=preview        " Not to show preview window on complection

set title
set titlestring=%t  " tile

set exrc    " Allow vim search local configuratin files in project filders.
set secure  " Disallows the use of :autocmd, shell and write commands in
            " local exrc files.

" }}}

" Python support                                                    {{{
" =====================================================================
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

" Clipboard                                                        {{{
" ====================================================================
if has('wsl')
" if has('unix')
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
endif
" }}}

" Plugins                                          {{{
" ====================================================
if has('unix')
    source ~/.config/nvim/plugins.vim
elseif has('win32')
    source ~\AppData\Local\nvim\plugins.vim
endif
" }}}

"  Indentation                                                      {{{
" =====================================================================
set autoindent     " Копировать отступы с текущей строки при
                   " добавлении новой.
set smartindent    " Включить “умную” расстановку отступов
set smarttab       " “Умная” расстановка отступов
set expandtab      " Заменять табуляцию пробелами
set tabstop=4      " Количество пробелов в одном символе табуляции
set softtabstop=4
set shiftwidth=4   " Количество пробелов на которое будет сдвинута
                   " строка командами >> или <<

filetype indent plugin on

" Show invisible symbols. Display tabs and trailing spaces visually
set list listchars=tab:\ \ ,trail:· ",eol:¶ ",eol:¤

" }}}

" Folds                                                             {{{
" =====================================================================
set foldenable         " fold by default
set foldmethod=marker   " fold based on markers
" set foldmethod=indent   " fold based on indent

set foldnestmax=3       " deepest fold is 3 levels (only for sintax and indent)

set foldtext=CustomFoldText('·')

function! CustomFoldText(string) " {{{
    " Idea by:
    " http://www.gregsexton.org/2011/03/improving-the-text-displayed-in-a-fold/

    "get first non-blank line
    let fs = v:foldstart
    if getline(fs) =~ '^\s*$'
      let fs = nextnonblank(fs + 1)
    endif

    if fs > v:foldend
        let line = getline(v:foldstart)
    else
        let line = substitute(getline(fs), '^\s*', '', '')
        let line = substitute(line, '\t', repeat(' ', &tabstop), 'g')
    endif

    " extract the part that defines the beginning of the comment string
    let pat  = matchstr(&l:commentstring, '^\V\.\{-}\ze%s\m')

    " remove leading comments from line
    let line = substitute(line, '^\s*'.pat.'\s*', '', '')

    " remove foldmarker from line
    let pat  = '\s*\%('. pat. '\)\?\s*'. split(&l:fmr, ',')[0]. '\s*\d*'
    let line = substitute(line, pat, '', '')

    if strlen(line) != 0
        let line = ' '. line .' '
        " let line = '| '. line .' |'
    endif

    let foldLevelStr = '+'. repeat(a:string, strlen(v:folddashes))

    " Size of the colummn with numbers of string (number column size).
    let nu = (&number ? strlen(string(line('$'))) + 5 : 4)
    let w = get(g:, 'custom_foldtext_max_width', winwidth(0)) - &foldcolumn - nu

    " let w = get(g:, 'custom_foldtext_max_width', winwidth(0)) - &foldcolumn - (&number ? 8 : 4)
    let foldSize = 1 + v:foldend - v:foldstart
    let foldSizeStr = " " . foldSize . " lines "
    let lineCount = line("$")

    " try
    "     let foldPercentage = printf("[%.1f", (foldSize*1.0)/lineCount*100) . "%] "
    " catch /^Vim\%((\a\+)\)\=:E806/	" E806: Using Float as String
    "     let foldPercentage = printf("[of %d lines] ", lineCount)
    " endtry

    let foldPercentage = ''

    let expansionString = repeat(a:string, w - strdisplaywidth(foldSizeStr.line.foldLevelStr.foldPercentage))

    " return foldLevelStr . '|' . line . '|' . expansionString . foldSizeStr . foldPercentage
    return foldLevelStr . line . expansionString . foldSizeStr . foldPercentage

endfunction
" }}}

" }}}

" Formating text                                                    {{{
" =====================================================================
" The behavior of 'gw' command
set formatoptions=tcqj  " Default value is 'tcqj'

set joinspaces   " Put two spaces after period.
set linebreak    " Wrap lines at convenient points

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

" =================== Completion =====================

" При автодополнении в командном режиме (:command) в начале списка
" показывать самый длинный вариант
" set wildmode=list:longest

set wildmenu  " enable ctrl-n and ctrl-p to scroll through matches

" Символ, который активирует автодополнение в скриптовых коммандах или при
" назначении клавиш. Использование <Tab> в этих ситуациях непосредственно
" вставит символ табуляции, а не откроет меню дополнения.
set wildcharm=<C-z>

" Отключение опции 'pum' включает автодополнение в командной строке
" в старом стиле: в строку а не в формате вертикального выпадающего списка.
set wildoptions=pum,tagfile
" set wildoptions=tagfile

" ================== Pop-Up Menu ====================

set pumblend=7          " Прозрачность всплывающего меню
set pumheight=15        " Количество строк во всплывающем окне

" =================== Scrolling ======================

" Start scrolling when we're n lines away from margins
set scrolloff=0

" Сколько колонок должно остаться до конца экрана, чтобы Vim начал
" прокручивать экран вбок
set sidescrolloff=1
" минимальное количество колонок на которое экран будет прокручиваться
" горизонтально за раз
set sidescroll=1

" ===================== Search =======================

set incsearch    " Подсвечивать найденный текст по мере набора
set hlsearch     " Highlight searches by default
set ignorecase   " Ignore case when searching...
set smartcase    " ...unless we type a capital

" ==================== Spelling ======================

" set spelllang=ru,en
set spelllang=ru_ru,en_us
" set dictionary            " используемые словари

" Автоматически включать проверку орфографии для определённых типов файлов
" autocmd BufRead,BufNewFile *.md setlocal spell
autocmd FileType text      setlocal spell
autocmd FileType gitcommit setlocal spell

" ===================== Syntax ========================

let g:markdown_fenced_languages = ['html', 'python', 'shell=sh', 'bash=sh', 'json']
let g:markdown_minlines = 100  " Syntax highlight is synchronized in 100 lines.

" ============== Turn Off Swap Files ==================

set noswapfile

" Needed for coc.nvim: some LSP servers have issues with
" backup files, see #649.
set nobackup
set nowritebackup

" ================== Русский язык =====================

set keymap=russian-jcukenwin
set iminsert=0 " Чтобы при старте ввод был на английском, а не русском (start > i)
set imsearch=0 " Чтобы при старте поиск был на английском, а не русском (start > /)

" Менять цвет курсора при включенном русском языке
" highlight Cursor guifg=Cyan guibg=Green ctermbg=Blue ctermfg=Cyan
" highlight lCursor guifg=NONE guibg=green

" highlight Cursor guifg=NONE guibg=#e8ae3c
" highlight lCursor guifg=NONE guibg=#e7ae3c

set helplang=ru      " Помощь на русском языке

" ================= Visual Tweaks ===================

set wrap             " Wrap lines
set cmdheight=1      " Make command line one line high
set colorcolumn=+1   " Показывать рулетку в следующей колонке после tw
set mousehide        " Hide the mouse when typing text
set cursorline       " выделять строку, на которой находится курсор

" set guicursor=a:blinkon100    " Turn on cursor blinking

" hi Cursor guifg=NONE guibg=cyan
" hi Cursor2 guifg=NONE guibg=red

" Change cursor shape between modes
set guicursor=n-v-c:block-Cursor/lCursor
set guicursor+=i-ci-ve:ver25-Cursor2/lCursor2,r-cr:hor20,o:hor50

" ================= Window Splits ====================

" Open splits on the left and above
set splitbelow
set splitright


" Autocommands and Functions                                         {{{
" ======================================================================

" https://www.bobbywlindsey.com/2017/07/30/vim-functions/
" convert rows of numbers or text (as if pasted from excel column) to a tuple
command! -range ToTuple <line1>,<line2> call ToTupleFunction()
function! ToTupleFunction() range  " {{{
    silent execute a:firstline . "," . a:lastline . "s/^/'/"
    silent execute a:firstline . "," . a:lastline . "s/$/',/"
    silent execute a:firstline . "," . a:lastline . "join"
    silent execute "normal I("
    silent execute "normal $xa)"
    silent execute "normal ggVGYY"
endfunction " }}}


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
autocmd FileType man if (winnr("$") != 1) | wincmd H | endif

" Automatically resize splits when resizing window
autocmd VimResized * wincmd =

" autocmd BufRead,BufNewFile *.txt setlocal textwidth=78

" Open Startify buffer in new tab page
" autocmd BufEnter * if !exists('t:startified') | Startify | let t:startified = 1 | endif

" Open preview window in vertical split
" :autocmd WinEnter * if &previewwindow && winnr() > 1 | wincmd K | endif
" autocmd WinEnter * if &previewwindow | wincmd L | endif


" Auto define the suitable foldcolumn value
augroup AutoSetFoldColumn
    autocmd!
    autocmd BufRead * let b:maxFoldLevel = MaxFoldLevel()
    autocmd WinEnter,BufWinEnter,VimResized * call SetFoldColumn()
    autocmd User MyCustomCloseWindow call SetFoldColumn()
augroup END

function! SetFoldColumn() "{{{
    " echom '-----------'
    " для всех окон
    for window in range(1, winnr('$'))

        " Maximum fold level for buffer in window
        let mfl = getbufvar(winbufnr(window), 'maxFoldLevel')

        " If max fold level is 0, scip such window.
        if mfl == 0
            " echom 'win '. window .' mfl==0'
            call setwinvar(window, "&foldcolumn", 0)
            continue
        endif

        " Переменная w:bufnr хранит номер буфера открытого в данном окне.
        " Если такой переменной не существует, эта функция (SetFoldColumn)
        " создаст её.  Если значение этой пременной равно номеру буфера
        " открытого в данном окне, то текущая итерация цикла пропускается.
        " Или другими словами, это означает, что с прошлой проверки буфер в
        " окне не поменялся.
        " С w:wwid всё тоже самое, только она хранит значение ширины окна.
        if getwinvar(window, 'bufnr') == winbufnr(window)  &&
         \ getwinvar(window, 'wwid') == winwidth(window)
            " echom 'Win '. window .' scip check'
            continue
        else
            let w:bufnr = winbufnr(window)
            let w:wwid  = winwidth(window)
        endif

        " textwidth for window
        let tw = getbufvar(winbufnr(window), '&textwidth')

        " Size of the column with numbers of lines.
        " '+1' is because there is always one space between column with line
        " numbers and the beginning of the text.
        let nu = (&number ?  strlen(string(line('$'))) + 1 : 0)

        " If window in narrower than textwidth + size of the number
        " column, then skip such window.
        if winwidth(window) <= (tw + nu)
            " echom 'win '.window.' width <= tw + nu'
            call setwinvar(window, "&foldcolumn", 0)
            continue
        endif

        " Required foldcolumn size
        let fdcSize = (mfl>1 ? mfl+1 : mfl)

        " The difference between the width of the window and
        " the textwidth in this window.
        let delta = abs(winwidth(window) - tw - nu)

        " Finale foldcolumn size
        let fdc = (fdcSize > delta ? delta : fdcSize)

        call setwinvar(window, "&foldcolumn", fdc)

        " echom 'win '. window .' fdc='. fdc

    endfor
endfunction
"}}}
function! MaxFoldLevel() " {{{
    " Retrun a number which is the maximum fold level of the current file.

    let winview = winsaveview()  " save window and cursor positions

    " open all nested folds
    norm zR
    1  " Jump to the first line of the buffer
    let position = line('.')
    let mfl = foldlevel(position)  " mfl: max fold level

    " jump to the next fold if exists
    norm zj

    while line('.') != position
        if foldlevel(position) > mfl
            let mfl = foldlevel(position)
        endif
        let position = line('.')
        norm zj
    endwhile

    " close all nested folds back
    norm zM
    call winrestview(winview)  " restore the window and cursor positions

    return mfl
endfunction
"}}}

" Tmux windows names
" autocmd BufReadPost,FileReadPost,BufNewFile * call system("tmux rename-window " . expand("%"))
" autocmd BufEnter * let &titlestring = ' ' . expand("%:t")
" set title

" }}}

" Key Bindings                                         {{{
" ========================================================
" Синтаксис создания комбинации
" [[mode]nore]map keys command
" где mode — режим или окружение, где работает комбинация
" nore (non-recursive) — не «раскрывать» комбинацию,
" а использовать значение по умолчанию
" ========================================================

" Fix writing :W to save
command! W w

" Копировать, вставить
vnoremap <silent> <C-c> "+y :let @*=@+<CR>
map <C-p> "+p

" If cursor is inside very long line in the file than wraps around
" several rows on the screen, then 'j' key moves you to the next line
" in the file, but not to the next row on the screen under your
" previous position as in other editors.  These bindings fixes this.
nmap j gj
nmap k gk

" noremap <C-Left>

" Перемещаться в начало / конец строки по 'Shift + h/l'
noremap H ^
noremap L $

" Отключить подсветку только что найденного текста
nnoremap <silent> <Esc> :noh<CR><Esc>


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
inoremap <expr> <Enter> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"


" часто нужно отделять блоки кода пустой строкой, например, перед return
nnoremap 2o o<CR>
nnoremap 2O O<Esc>O

" WARNING: Replaced with 'christoomey/vim-tmux-navigator' plugin
" " Quick jumping between splits
" map <C-J> <C-W>j
" map <C-K> <C-W>k
" map <C-H> <C-W>h
" map <C-L> <C-W>l

nnoremap <A-Up> <C-W>+
nnoremap <A-Down> <C-W>-
nnoremap <A-Left> <C-W><
nnoremap <A-RIght> <C-W>>

" Open new splits easily
map vv <C-W>v
" map ss <C-W>s


" Close preview window if exists, else close current window
map <silent> Q :call CloseWindow()<CR>

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
endfunction
"}}}

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
endfunction
"}}}


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


" Save changes in buffer
nmap <silent> <Leader>s :write<Enter>


" открыть файл конфигурации .vimrc или init.vim
command! I edit $MYVIMRC
" открыть файл с плагинами
if has('unix')
    command! P edit ~/.config/nvim/plugins.vim
endif

" Включать и выклюяать проверку орфографии
map <silent> <F6> :call ToggleSpell()<CR>

fu! ToggleSpell()  " {{{
    " Toggle the spell state
    setlocal spell!
    " echo &spell ? 'spell' : 'nospell'
    echo &spell ? 'spell on' : 'spell off'
endf
" }}}


" Переключать язык по нажатию Ctrl-\ вместо Ctrl-^, как в Emacs.
" Switch keyboard layout (language) on Ctrl-\ instead of Ctrl-^,
" like in Emacs.
" ----------------------------------------------------------
inoremap <silent> <C-\> <C-O>:call ToggleKeyboardLayout()<CR>
nnoremap <silent> <C-\> :call ToggleKeyboardLayout()<CR>

" " Switch keyboard layout (language) on Ctrl-Space instead of Ctrl-^.
" " ----------------------------------------------------------
" inoremap <silent> <C-Space> <C-O>:call ToggleKeyboardLayout()<CR>
" nnoremap <silent> <C-Space> :call ToggleKeyboardLayout()<CR>

fu! ToggleKeyboardLayout()  "{{{
    if &iminsert == 0
        " set iminsert=1
        setlocal iminsert=1
    elseif &iminsert == 1
        " set iminsert=0
        setlocal iminsert=0
    endif
endf  "}}}

" }}}

" vim: fen fdm=marker number tw=76 cc=+1
