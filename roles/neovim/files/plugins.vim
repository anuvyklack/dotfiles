
"         ███                 ██
"        ░░██                ░░
"  ██████ ░██ ██   ██  ██████ ██ ██████   ██████
" ░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░
" ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████
" ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██
" ░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████
" ░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░
" ░░                  ░░░░░


" Install Vim-Plug in Unix if not yet                               {{{
" =====================================================================
if has('unix')

    let vimplug_exists=expand('~/.config/nvim/autoload/plug.vim')

    if !filereadable(vimplug_exists)
        if !executable("curl")
            echoerr "You have to install curl or first install vim-plug yourself!"
            execute "q!"
        endif
        echo "Installing Vim-Plug..."
        echo ""
        silent exec "!\curl -fLo " . vimplug_exists . " --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

        autocmd VimEnter * PlugInstall
    endif

endif
" }}}

if has('unix')
    call plug#begin(expand('~/.config/nvim/plugged'))
elseif has('win32')
    call plug#begin(expand('~\AppData\Local\nvim\plugged'))
endif

" -------------------- Прочее ------------------------

Plug '~/.config/nvim/plugged/myhelp'

" Plug 'wellle/context.vim'    " Vscode breadcrumbs analog

Plug 'majutsushi/tagbar'     " список тегов в текущем файле
" Plug 'kien/tabman.vim'       " Tab management for Vim

Plug 'kshenoy/vim-signature' " display and navigate marks

" Show syntax highlighting attributes of character under cursor.
Plug 'vim-scripts/SyntaxAttr.vim'

" Airline                                                           {{{
" =====================================================================

" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'
" so ~/.config/nvim/plugins_settings/airline.vim

" }}}

" Beautiful Lightline configuration                                 {{{
" =====================================================================
" https://gist.github.com/sainnhe/b8240bc047313fd6185bb8052df5a8fb

Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'
Plug 'macthecadillac/lightline-gitdiff'
Plug 'maximbaz/lightline-ale'
Plug 'albertomontesg/lightline-asyncrun'
Plug 'rmolin88/pomodoro.vim'
so ~/.config/nvim/plugins_settings/lightline.vim

" Pomodoro                                                          {{{
" ---------------------------------------------------------------------
let g:pomodoro_time_work = 2
let pomodoro_use_devicons = 1
" }}}

" }}}

" " ALE                                              {{{
" " ----------------------------------------------------
" " Asynchronous Lint Engine -- is a plugin providing linting (syntax
" " checking and semantic errors) while you edit your text files, and acts
" " as a Vim Language Server Protocol client.
" Plug 'dense-analysis/ale'

" let g:ale_linters = {
"       \   'python': ['flake8', 'pylint'],
"       \   'ruby': ['standardrb', 'rubocop'],
"       \   'javascript': ['eslint'],
"       \}

" " Some of the linters are also capable of fixing the problems in your code.
" " ALE has a special command :ALEFix that fixes the whole file. So far, I'm
" " only Google's YAPF as a fixer that formats the whole file when I press
" " F10 or save the current buffer.
" let g:ale_fixers = {
"       \    'python': ['yapf'],
"       \}
" nmap <F10> :ALEFix<CR>
" let g:ale_fix_on_save = 1

" " I also have a little piece of configuration that shows the total number of
" " warnings and errors in the status line. Very convenient.
" function! LinterStatus() abort
"   let l:counts = ale#statusline#Count(bufnr(''))
"
"   let l:all_errors = l:counts.error + l:counts.style_error
"   let l:all_non_errors = l:counts.total - l:all_errors
"
"   return l:counts.total == 0 ? '✨ all good ✨' : printf(
"         \   '😞 %dW %dE',
"         \   all_non_errors,
"         \   all_errors
"         \)
" endfunction
"
" set statusline=
" set statusline+=%m
" set statusline+=\ %f
" set statusline+=%=
" set statusline+=\ %{LinterStatus()}

" }}}

" ------------ Completion Autocomplete ---------------

" Plug 'ervandew/supertab'

" Plug 'Valloric/YouCompleteMe'
" so ~/.config/nvim/plugins_settings/YouCompleteMe.vim

" Coc.nvim {{{

Plug 'neoclide/coc.nvim', {'branch': 'release'}
so ~/.config/nvim/plugins_settings/coc.vim

" }}}

" Buffexplorer  {{{
Plug 'jlanzarotta/bufexplorer'
let g:bufExplorerFindActive=0   " Do not go to active window.
" }}}

" --------------- Text manipulations -----------------

Plug 'jiangmiao/auto-pairs'  " автоматическое завершение скобок
Plug 'matze/vim-move'        " перемещение строк и частей строк
Plug 'tpope/vim-surround'    " заключать фрагменты текста в кавычки или скобки

Plug 'wellle/targets.vim'    " plugin that provides additional text objects
" Plug 'Konfekt/FastFold'      " Speed up Vim by updating folds only when called-for

" Переключение мемжду многострочными и однострочными конструкциями
Plug 'AndrewRadev/splitjoin.vim'

Plug 'godlygeek/tabular'     " Выравнивание текста по различным шаблонам
Plug 'junegunn/vim-easy-align'  " alignment plugin

" --------------- Visual improvements ----------------

" Plug 'Yggdroot/indentLine'     " show indent lines
" Plug 'nathanaelkane/vim-indent-guides'

" Подсвечивает все такие же слова как и слово под курсором
Plug 'RRethy/vim-illuminate'

Plug 'inside/vim-search-pulse' " Найденный текст пульсирует

" Подсвечивать и удалять висящие пробелы в конце строк
Plug 'ntpeters/vim-better-whitespace'
so ~/.config/nvim/plugins_settings/vim-better-whitespace.vim

" ----------------------------------------------------

" Colorizer                                                          {{{
" ======================================================================
" Подсвечивает цветовые коды соответствующими цветами
Plug 'lilydjwg/colorizer', { 'on': ['ColorHighlight'] }

"   ColorHighlight  - start/update highlighting
"   ColorClear      - clear all highlights
"   ColorToggle     - toggle highlights

" Запускать подсветку цветоввых кодов на старте
let g:colorizer_startup = 0
" }}}

" Comments                                                           {{{
" ======================================================================
" Plug 'tpope/vim-commentary'  " добавляет оператор вместо набора команд

" tcomment                                                          {{{2
" ----------------------------------------------------------------------
Plug 'tomtom/tcomment_vim'

" }}}2

" NERDCommenter                                                     {{{2
" ----------------------------------------------------------------------

" Plug 'scrooloose/nerdcommenter'  " комментарии всевозможных видов и форм
"
" " so ~/.config/nvim/plugins_settings/NERDCommenter.vim
"
" let g:NERDSpaceDelims = 1        " add spaces around comments
" let g:NERDRemoveExtraSpaces = 1  " remove extra spaces around comments
"
" " Комменировать пустые строки при комментировании блоков текста
" let g:NERDCommentEmptyLines = 1
"
" " Use compact syntax for prettified multi-line comments
" let g:NERDCompactSexyComs = 1

" }}}2

" }}}

" "  fzf                                              {{{
" " =====================================================
" if has('unix')
"
"     if filereadable(expand("~/.zinit/snippets/fzf.vim/fzf.vim"))
"         " If fzf was installed by zinit
"         source ~/.zinit/snippets/fzf.vim/fzf.vim
"     elseif filereadable("/usr/share/doc/fzf/examples/fzf.vim")
"         " If fzf was installed through apt.
"         source /usr/share/doc/fzf/examples/fzf.vim
"     endif
"     Plug 'junegunn/fzf.vim'
"
"     " Fzf in a floating window  {{{
"     " https://kassioborges.dev/2019/04/10/neovim-fzf-with-a-floating-window.html
"
"     " Reverse the layout to make the FZF list top-down
"     let $FZF_DEFAULT_OPTS='--layout=reverse'
"
"     " Using the custom window creation function
"     let g:fzf_layout = { 'window': 'call FloatingFZF()' }
"
"     " Function to create the custom floating window
"     function! FloatingFZF()
"       " creates a scratch, unlisted, new, empty, unnamed buffer
"       " to be used in the floating window
"       let buf = nvim_create_buf(v:false, v:true)
"
"       " 90% of the height
"       let height = float2nr(&lines * 0.7)
"       " 60% of the height
"       let width = float2nr(&columns * 0.6)
"       " horizontal position (centralized)
"       let horizontal = float2nr((&columns - width) / 2)
"       " vertical position (one line down of the top)
"       let vertical = 1
"
"       " Set the position, size, etc. of the floating window.
"       " The size configuration here may not be so flexible, and there's
"       " room for further improvement.
"       let opts = {
"             \ 'relative': 'editor',
"             \ 'row': vertical,
"             \ 'col': horizontal,
"             \ 'width': width,
"             \ 'height': height
"             \ }
"
"       " open the new window, floating, and enter to it
"       " call nvim_open_win(buf, v:true, opts)
"       let win = nvim_open_win(buf, v:true, opts)
"
"       "Set Floating Window Highlighting
"       " call setwinvar(win, '&winhl', 'Normal:Pmenu')
"
"       setlocal
"             \ buftype=nofile
"             \ nobuflisted
"             \ bufhidden=hide
"             \ nonumber
"             \ norelativenumber
"             \ signcolumn=no
"
"     endfunction "}}}
"
" endif
" " }}}

" LeaderF                                           {{{
" =====================================================
" After running any command of LeaderF, check the value
" of echo g:Lf_fuzzyEngine_C, if the value is 1, it means
" the C extension is loaded sucessfully.

if has('unix')
    Plug 'Yggdroot/LeaderF', { 'do': './install.sh' }
    so ~/.config/nvim/plugins_settings/LeaderF.vim
elseif has('win32')
    Plug 'Yggdroot/LeaderF', { 'do': '.\install.bat' }
endif

" }}}

"  Git                                             {{{
" ====================================================

Plug 'tpope/vim-fugitive'  " git integration

" Plug 'mhinz/vim-signify'  " значки с историей изменений слева от текста

Plug 'junegunn/gv.vim'  " A git commit browser in Vim.

" }}}

" --------------- Motions in window ------------------

" Easymotion {{{
Plug 'easymotion/vim-easymotion'
so ~/.config/nvim/plugins_settings/easymotion.vim
" }}}

Plug 'tpope/vim-repeat'

" Clever-f {{{
Plug 'rhysd/clever-f.vim'
let g:clever_f_ignore_case = 1
let g:clever_f_smart_case = 1
let g:clever_f_show_prompt = 1
let g:clever_f_chars_match_any_signs = ';'
" }}}

" Plug 'chaoren/vim-wordmotion'  " More useful word motions for Vim

" Different bidirectional motions: switch buffers, add balnk lines, etc.
Plug 'tpope/vim-unimpaired'

" Clever % command
Plug 'andymass/vim-matchup'

" --------------------- Fold ------------------------
" Plug 'arecarn/vim-fold-cycle'
" Plug 'benknoble/vim-auto-origami'

" IDE features                                     {{{
" ====================================================

Plug 'liuchengxu/vista.vim'  " View and search LSP symbols and tags.

" WARNING: Not working!
" Plug 'pechorin/any-jump.vim'

" }}}

" Multiple cursors                                 {{{
" ====================================================
Plug 'terryma/vim-multiple-cursors'
" Plug 'mg979/vim-visual-multi', {'branch': 'master'}
" }}}

" Pandoc                                                        {{{
" =================================================================
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
so ~/.config/nvim/plugins_settings/pandoc.vim
" }}}

" Python                                                            {{{
" =====================================================================

" " Python mode
" Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }
" so ~/.config/nvim/plugins_settings/python-mode.vim

" Plug 'vim-python/python-syntax', { 'for': 'python' }
" let g:python_highlight_all = 1

" pudb python degugger integration
Plug 'SkyLeach/pudb.vim', { 'for': 'python' }

" Provides some Python-specific text objects and motions for classes,
" methods, functions, and doc strings.
Plug 'jeetsukumaran/vim-pythonsense', { 'for': 'python' }

" Indentation behavior that complies with PEP8.
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }

" Python code folding for Vim
Plug 'tmhedberg/SimpylFold', { 'for': 'python' }
let g:SimpylFold_fold_import = 0

" Semantic based code highlighting
" Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins', 'for': 'python'}
" so ~/.config/nvim/plugins_settings/semshi.vim

" Connection to Jupyter QtConsole
" Plug 'broesler/jupyter-vim'
" let g:jupyter_mapkeys = 1  " enable default key mapping

" Looks like it doesn't works with Neovim
" Plug 'williamjameshandley/vimteractive'  " connection with ipython

" Plug 'szymonmaszke/vimpyter'  " edit ipython or ntrect notebooks in vim

" =====================================================================
" }}}

" Rainbow  {{{
" расцвечивание скобок по уровню вложенности
Plug 'luochen1990/rainbow'
" so ~/.config/nvim/plugins_settings/rainbow.vim
let g:rainbow_active = 1

" " Default: '#c475c1', '#8ab7d8', '#60dd60', '#ffff70', '#ea9d70', '#971717'
" " My changes: #7ab061
" let g:rainbow_conf = {
" \   'guifgs': ['#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'],
" \   'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
" \   'separately': { 'nerdtree': 0, 'pandoc': 0 }
" \}

if !exists('g:rainbow_conf')
  let g:rainbow_conf = {}
endif
let g:rainbow_conf.separately = { 'nerdtree': 0, 'pandoc': 0 }
" let g:rainbow_conf.separately.lisp = {'guifgs': ['#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717'] }
let g:rainbow_conf.separately.lisp = {'guifgs': ['#f28534', '#f2594b', '#80aa9e', '#e9b143', '#b0b846', '#d3869b'] }

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

" if !exists('g:rbpt_colorpairs')
"   let g:rbpt_colorpairs = [['blue', s:palette.blue[0]], ['magenta', s:palette.purple[0]],
"         \ ['red', s:palette.red[0]], ['166', s:palette.orange[0]]]
" endif

" let g:rainbow_guifgs = [ s:palette.orange[0], s:palette.red[0], s:palette.purple[0], s:palette.blue[0] ]
" let g:rainbow_ctermfgs = [ '166', 'red', 'magenta', 'blue' ]

" if !has_key(g:rainbow_conf, 'guifgs')
"   let g:rainbow_conf['guifgs'] = g:rainbow_guifgs
" endif
" if !has_key(g:rainbow_conf, 'ctermfgs')
"   let g:rainbow_conf['ctermfgs'] = g:rainbow_ctermfgs
" endif

" }}}

" ---------------------- Tags ------------------------

" Автоматически генерировать тэг-файлы
Plug 'ludovicchabant/vim-gutentags'

" " --------------------- Wintabs ----------------------
"
" " TODO: разберёмся с этим позже
" " Plug 'zefei/vim-wintabs'
" " Plug 'zefei/vim-wintabs-powerline'"

Plug 'roxma/vim-window-resize-easy'

" ------------------ Wrighting -----------------------

Plug 'lervag/wiki.vim'
so ~/.config/nvim/plugins_settings/wiki.vim

" Plug 'vimwiki/vimwiki'      " Wiki inside Vim!
" so ~/.config/nvim/plugins_settings/vimwiki.vim

Plug 'jceb/vim-orgmode'     " Orgmode
" so ~/.config/nvim/plugins_settings/orgmode.vim

Plug 'junegunn/goyo.vim'    " Beautiful regime for writing

" " Pencil                                                            {{{
" " ---------------------------------------------------------------------
" Plug 'reedes/vim-pencil'  " Writing tools: soft wrap end etc
"
" let g:pencil#wrapModeDefault = 'soft'  " default is 'hard'
"
" " Only work in HardPencil mode;
" let g:pencil#autoformat = 1  " 0=disable, 1=enable (def)
" let g:pencil#textwidth = 80
"
" let g:pencil#joinspaces = 1  " 0=one_space (def), 1=two_spaces
"
" augroup pencil
"     autocmd!
"     autocmd FileType pandoc       call pencil#init()
"     autocmd FileType markdown,mkd call pencil#init()
"     autocmd FileType text         call pencil#init({'wrap': 'hard'})
" augroup END
"
" "}}}

" ----------------------------------------------------

" Ui menus, listboxes, textboxes and else.
" Settings of this plugin should be out of Vim-Plug 'plug#begin() ...
" plug#end()' area, so they are at the end of the file.
Plug 'skywind3000/vim-quickui'

" Smooth scroll                                    {{{
" ====================================================

Plug 'psliwka/vim-smoothie'

" Time (in milliseconds) between subseqent screen/cursor postion updates.
" Lower value produces smoother animation.
let g:smoothie_update_interval = 20

" Base scrolling speed (in lines per second), to be taken into account by
" the velocity calculation algorithm.  Can be decreased to achieve slower
" (and easier to follow) animation.
let g:smoothie_base_speed = 7

" " Accelerated smooth scroll {{{
" Plug 'yonchu/accelerated-smooth-scroll'
" let g:ac_smooth_scroll_enable_accelerating = 0
" " }}}

" Comfortable-Motion {{{

" Plug 'yuttie/comfortable-motion.vim'
"
" " To prevent the plugin from defining those default key mappings
" let g:comfortable_motion_no_default_key_mappings = 1
"
" let g:comfortable_motion_interval = 17
"
" " mouse wheel to scroll a window by the following mappings:
" noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
" noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>

" }}}

" }}}

" Startify                                                         " {{{
" ======================================================================
Plug 'mhinz/vim-startify'    " Красивый стартовый экран
so ~/.config/nvim/plugins_settings/startify.vim

" }}}

" C/C++                                                              {{{
" ======================================================================

Plug 'octol/vim-cpp-enhanced-highlight'  " c++ highlight
so ~/.config/nvim/plugins_settings/vim-cpp-enhanced-highlight.vim

" Use cppman for c++ documentations
" Plug 'gauteh/vim-cppman'
Plug 'anuvyklack/vim-cppman'

" }}}

" Different syntaxes and languages                                   {{{
" ======================================================================
Plug 'sheerun/vim-polyglot'  " подсветка синтаксисов разных языков
let g:polyglot_disabled = ['markdown']

Plug 'lervag/vimtex', { 'for': 'LaTeX' }  " latex
Plug 'PProvost/vim-ps1', {'for': 'ps1'}   " powershell
Plug 'zinit-zsh/zinit-vim-syntax', { 'for': 'zsh' }  " zinit syntaxis

" }}}

" Русский язык (Switch language)                                     {{{
" ======================================================================

" Plug 'lyokha/vim-xkbswitch'
"
" let g:XkbSwitchEnabled = 1
" if has('wsl')
"     let g:XkbSwitchLib = '/mnt/c/tools/libxkbswitch64.dll'
" elseif has('win32')
"     let g:XkbSwitchLib = 'C:\tools\libxkbswitch64.dll'
" endif

Plug 'powerman/vim-plugin-ruscmd'

" }}}

" Undotree                                                          {{{
" =====================================================================
Plug 'mbbill/undotree'         " visualize undo tree
" Plug 'simnalamburt/vim-mundo'  " another undo tree visualizer

let g:undotree_HighlightChangedWithSign = 0
let g:undotree_WindowLayout             = 2

" }}}

" NERDTree                                                           {{{
" ======================================================================
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeFind' }
so ~/.config/nvim/plugins_settings/NERDTree.vim

" Добавляет цветную подсветку к иконкам
Plug 'vwxyutarooo/nerdtree-devicons-syntax'


" " Показывать скрытые файлы по умолчанию
" let NERDTreeShowHidden = 0
"
" " Automatically close NerdTree when you open a file
" let NERDTreeQuitOnOpen = 0
"
" " Automatically delete the buffer of the file you just deleted with NerdTree
" let NERDTreeAutoDeleteBuffer = 1
"
" " disable “Press ? for help”
" let NERDTreeMinimalUI = 0
"
" let g:NERDTreeHijackNetrw = 1
"
" let g:NERDTreeDirArrowExpandable  = "▷"
" let g:NERDTreeDirArrowCollapsible = "◢"
"
" " Close vim if the only window left open is a NERDTree
" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" }}}

" Devicons                                        {{{
" ---------------------------------------------------

Plug 'ryanoasis/vim-devicons'

" so ~/.config/nvim/plugins_settings/devicons.vim

let g:webdevicons_conceal_nerdtree_brackets = 1

" Reload Vim-Deviconda after reload .vimrc file
if exists("g:loaded_webdevicons")
    call webdevicons#refresh()
endif

" }}}

" Unused {{{

" Plug 't9md/vim-choosewin'    " Switch Windows on choose
" let g:choosewin_overlay_enable = 1 " use overlay
" nmap - <Plug>(choosewin)

" Delete buffer without closing related window
" Plug 'qpkorr/vim-bufkill'

" 'Rich text' highlighting in Vim
" (colors, underline, bold, italic, etc...)
" Plug 'bpstahlman/txtfmt'
" so ~/.config/nvim/plugins_settings/txtfmt.vim

" }}}

" Color Themes                                     {{{
" ====================================================

" Plug 'ayu-theme/ayu-vim'
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'
" Plug 'mhartington/oceanic-next'
" Plug 'ajmwagar/vim-deus'
Plug 'sainnhe/gruvbox-material'
" }}}

call plug#end()

"                 ███                                    ██
"                ░░██                                   ░██
"   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
"  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
" ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
" ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
" ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
"  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░

" let s:theme = 'ayu'
" let s:theme = 'onedark'
" let s:theme = 'gruvbox'
let s:theme = 'gruvbox-material'
" let s:theme = 'gruvbox-8'
" let s:theme = 'deus'
" let s:theme = 'OceanicNext'

" Onedark {{{
if s:theme == 'onedark'
    let g:onedark_hide_endofbuffer = 1
    let g:onedark_terminal_italics = 1  " italic for comments

    let g:onedark_color_overrides = {
    \   "comment_grey": { "gui": "#666e7d", "cterm": "59", "cterm16": "15" },
    \}

    let s:colors = onedark#GetColors()

    if (has("autocmd"))
      augroup colorextend
        autocmd!
        " Override Foldcolumn color
        autocmd ColorScheme * call onedark#extend_highlight("FoldColumn", { "fg": s:colors.comment_grey })
      augroup END
    endif

    colorscheme onedark
    let g:airline_theme='onedark'
    if exists('g:lightline')
        let g:lightline.colorscheme = 'onedark'
    endif

    " Default: '#c475c1', '#8ab7d8', '#60dd60', '#ffff70', '#ea9d70', '#971717'
    " My changes: #7ab061
    let g:rainbow_conf.guifgs = ['#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717']
    let g:rainbow_conf.ctermfgs = ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta']

" }}}
" Gruvbox {{{
elseif s:theme == 'gruvbox'
    let g:gruvbox_improved_strings = 1
    let g:gruvbox_contrast_dark = 'medium'
    colorscheme gruvbox
    let g:airline_theme = 'gruvbox'
    if exists('g:lightline')
        let g:lightline.colorscheme = 'gruvbox'
    endif
" }}}
" Ayu {{{
elseif s:theme == 'ayu'
    " let ayucolor="light"  " for light version of theme
    let ayucolor = "mirage" " for mirage version of theme
    " let ayucolor="dark"   " for dark version of theme
    colorscheme ayu
    let g:airline_theme = 'ayu'

    " ----------------- IndentLine -------------------
    let g:indentLine_char = '┊'
    let g:indentLine_first_char = '┊'
    " let g:indentLine_char_list = ['|', '¦', '┆', '┊']
    let g:indentLine_showFirstIndentLevel = 1
    let g:indentLine_setColors = 0
" }}}
" OceanicNext {{{
elseif s:theme == 'OceanicNext'
    colorscheme OceanicNext
    let g:airline_theme='oceanicnext'
" }}}
" Deus {{{
elseif s:theme == 'deus'
    colorscheme deus
" }}}
" Gruvbox-material {{{
elseif s:theme == 'gruvbox-material'

    " Set the color palette used in this color scheme.
        " material : material palette with soft contrast;
        " mix      : the mean of the other two;
        " original : the original gruvbox palette.
    let g:gruvbox_material_palette = 'mix'

    " set contrast
    " available values: 'hard', 'medium'(default), 'soft'
    let g:gruvbox_material_background = 'medium'
    let g:gruvbox_material_enable_bold = 1
    let g:gruvbox_material_enable_italic = 1

    " Available values: 'auto', 'red', 'orange', 'yellow',
    " 'green', 'aqua', 'blue', 'purple'
    " let g:gruvbox_material_cursor = 'aqua'
    " let g:gruvbox_material_cursor = 'orange'
    " let g:gruvbox_material_cursor = 'yellow'
    let g:gruvbox_material_cursor = 'blue'

    colorscheme gruvbox-material
    let g:airline_theme = 'gruvbox_material'
    if exists('g:lightline')
        let g:lightline.colorscheme = 'gruvbox_material'
    endif

    " " Default: '#c475c1', '#8ab7d8', '#60dd60', '#ffff70', '#ea9d70', '#971717'
    " " My changes: #7ab061
    " let g:rainbow_conf.guifgs = ['#c475c1', '#8ab7d8', '#98c369', '#ffff70', '#ea9d70', '#971717']
    " let g:rainbow_conf.ctermfgs = ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta']

endif
" }}}


"  ██                           ██      ██              ██ ██
" ░██                          ░██     ░░              ░██░░
" ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
" ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
" ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
" ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
" ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
" ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
"                  ░░░░░                                              ░░░░░

nnoremap <silent> <F1> :help myhelp.txt<CR>

" Vim-quickui plugin settings.
" Should be after the end of Vim-Plug call end.
so ~/.config/nvim/plugins_settings/vim-quickui.vim

" Easymotion key bindings {{{
map  ; <Plug>(easymotion-prefix)
nmap s <Plug>(easymotion-bd-f)
xmap s <Plug>(easymotion-bd-f)
omap s <Plug>(easymotion-bd-f)
nmap ;w <Plug>(easymotion-w)
nmap ;b <Plug>(easymotion-b)
nmap ;l <Plug>(easymotion-lineanywhere)
"}}}

" Windows scrolling options / Comfortable motion key bindings {{{

" " Little less then half of the screen
" nnoremap <silent> <C-d> :call comfortable_motion#flick(winheight(0) * 2)<CR>
" nnoremap <silent> <C-u> :call comfortable_motion#flick(winheight(0) * -2)<CR>
"
" " " Half of the screen
" " nnoremap <silent> <C-d> :call comfortable_motion#flick(winheight(0) * 2.2)<CR>
" " nnoremap <silent> <C-u> :call comfortable_motion#flick(winheight(0) * -2.2)<CR>
"
" " Full screen
" nnoremap <silent> <C-f> :call comfortable_motion#flick(winheight(0) * 3.5)<CR>
" nnoremap <silent> <C-b> :call comfortable_motion#flick(winheight(0) * -3.5)<CR>

" }}}

" Fzf {{{

" nmap <Leader>F :Files<CR>
" nmap <Leader>f :GFiles<CR>
" "
" nmap <Leader>h :History<CR>
" "
" nmap <Leader>l :BLines<CR>
" nmap <Leader>L :Lines<CR>
" nmap <Leader>' :Marks<CR>
" "
" nmap <Leader>/ :Rg<Space>
" "
" " Fuzzy search Vim help
" " nmap <Leader>H :Helptags!<CR>
" nmap <Leader>H :Helptags<CR>
" "
" nmap <Leader>C :Commands!<CR>
" "
" " Fuzzy search through ':command' history
" " nmap q: :History:!<CR>
" " nmap q/ :History/!<CR>
" nmap q: :History:<CR>
" nmap q/ :History/<CR>
" "
" " Fuzzy search key mappings
" nmap <Leader>M :Maps<CR>

" }}}

" LeaderF {{{

let g:Lf_ShortcutF = "<leader>ff"
nmap <silent> <Leader>fF :Leaderf file ~<CR>
"
nmap <leader>fh :Leaderf mru<CR>
"
nmap <leader>ft :Leaderf tag<CR>
nmap <leader>fu :Leaderf function<CR>
"
" nmap <Leader>fl :LeaderfLine<CR>
nmap <Leader>fl :Leaderf line<CR>
nmap <Leader>fL :LeaderfLineAll<CR>
" nmap <Leader>' :Marks<CR>
"
nmap <Leader>/ :LeaderfRgInteractive<CR>
"
" Fuzzy search Vim help
nmap <Leader>fH :LeaderfHelp<CR>
"
" nmap <Leader>C :Commands!<CR>
"
" Fuzzy search through ':command' history
nmap q: :LeaderfHistoryCmd<CR>
nmap q/ :LeaderfHistorySearch<CR>
"
" let g:Lf_ShortcutF = "<leader>ff"
" noremap <leader>fb :<C-U><C-R>=printf("Leaderf buffer %s", "")<CR><CR>
" noremap <leader>fm :<C-U><C-R>=printf("Leaderf mru %s", "")<CR><CR>
" noremap <leader>ft :<C-U><C-R>=printf("Leaderf bufTag %s", "")<CR><CR>
" noremap <leader>fl :<C-U><C-R>=printf("Leaderf line %s", "")<CR><CR>

" }}}

" Easy-Align {{{

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" apple   =red
" grass+=green
" sky-=   blue

" }}}

" Открыть файловую панель NERDTree, и установить в ней курсор на файле
" открытом в текущем буфере. Повторное нажатие закроет файловую панель.
nnoremap <silent> <F2> :call ToggleNERDTree()<CR>
function! ToggleNERDTree() " {{{
    if IsFileTypeOpen('nerdtree')
        NERDTreeClose
        " AirlineRefresh
    else
        NERDTreeFind
    endif
endfunction
" }}}

" noremap <silent> <F3> :SignatureToggleSigns<CR>
" noremap <silent> <F4> :SignatureListBufferMarks<CR>
noremap <silent> <F5> :GV<CR>


nnoremap <silent> gb :call ChooseBuffer()<CR>
function! ChooseBuffer()  "{{{
    " Количество открытых буферов
    let num_of_buffers = len(getbufinfo({'buflisted':1}))
    if num_of_buffers > 2
        " If you are interesting what is <C-z> check ':help wildcharm'.
        " call feedkeys(":buffer \<C-z>")
        ToggleBufExplorer
    else
        bnext
    endif
endfunction
"}}}


nnoremap <Leader>u :UndotreeToggle<CR>

nmap <leader>vw <Plug>(wiki-index)


" Показать syntax group для участка кода, а также цвет этой группы.
" Удобно при создании своей цветовой схемы
nnoremap <C-g> :call SyntaxAttr()<CR>

" nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>

" vim: foldenable tw=75 colorcolumn=+1
