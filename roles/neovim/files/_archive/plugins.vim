"
"         ███                 ██
"        ░░██                ░░
"  ██████ ░██ ██   ██  ██████ ██ ██████   ██████
" ░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░
" ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████
" ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██
" ░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████
" ░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░
" ░░                  ░░░░░


"            Install Vim-Plug if not yet           {{{
" ----------------------------------------------------
let vimplug_exists=expand('~/.config/nvim/autoload/plug.vim')
if has('win32')&&!has('win64')
  let curl_exists=expand('C:\Windows\Sysnative\curl.exe')
else
  let curl_exists=expand('curl')
endif

if !filereadable(vimplug_exists)
  if !executable(curl_exists)
    echoerr "You have to install curl or first install vim-plug yourself!"
    execute "q!"
  endif
  echo "Installing Vim-Plug..."
  echo ""
  silent exec "!"curl_exists" -fLo " . shellescape(vimplug_exists) . " --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  let g:not_finish_vimplug = "yes"

  autocmd VimEnter * PlugInstall
endif
" -------------------------------------------------}}}


if has('unix')
    call plug#begin(expand('~/.config/nvim/plugged'))
elseif has('win32')
    call plug#begin(expand('~\AppData\Local\nvim\plugged'))
endif

" --------------------- Прочее -----------------------

" My custom F1 help page
Plug '~/.config/nvim/plugged/myhelp'

" ------------------ Text editing --------------------

Plug 'jiangmiao/auto-pairs'  " автоматическое завершение скобок
Plug 'matze/vim-move'        " перемещение строк и частей строк
Plug 'tpope/vim-surround'    " заключать фрагменты текста в кавычки или скобки
Plug 'wellle/targets.vim'    " plugin that provides additional text objects
Plug 'kshenoy/vim-signature' " display and navigate marks
Plug 'tpope/vim-unimpaired'  " Different bidirectional motions: switch
                             " buffers, add blank lines, etc.

" -------------- Programming feature -----------------
" ------------------- Statusline ---------------------
" ------------ Completion (autocomplete) -------------
" ----------------- Visual tweaks --------------------

Plug 'RRethy/vim-illuminate'   " Подсвечивает все такие же слова
                               "   как и слово под курсором.
Plug 'inside/vim-search-pulse' " Найденный текст пульсирует

" -------------------- Comments ----------------------
Plug 'tomtom/tcomment_vim'

" ------------- Motions inside window ----------------

" ------------------- Easymotion ---------------------
Plug 'easymotion/vim-easymotion'
so ~/.config/nvim/plugins_settings/easymotion.vim
" ----------------------------------------------------

"                     Treesitter                   {{{
" ----------------------------------------------------
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/playground'
" -------------------------------------------------}}}

" --------- LSP (language server protocol) -----------

lua << EOS
-- For lsp the following has been working pretty well for me
-- Nvim-lspconfig (for loading language servers)
-- nvim-compe (for completion)
-- lsp-trouble (for viewing info)
-- And then come-tabnine as a tabnine source for compe + lspsaga for cool icons.
-- Its definitely not a unified experience, but it isn't very hard to put
-- together,
EOS

" ---------------- Multiple cursors ------------------
" Plug 'mg979/vim-visual-multi', {'branch': 'master'}
" ----------------------------------------------------

"                      Pandoc                      {{{
" ----------------------------------------------------
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
so ~/.config/nvim/plugins_settings/pandoc.vim
" -------------------------------------------------}}}

" --------- Windows and buffers managment ------------
Plug 'roxma/vim-window-resize-easy'

"                    Buffexplorer                  {{{
" ----------------------------------------------------
Plug 'jlanzarotta/bufexplorer'
let g:bufExplorerFindActive=0   " Do not go to active window.
" -------------------------------------------------}}}

" ----------------------------------------------------

"                  Smooth scroll                   {{{
" ----------------------------------------------------
" https://github.com/psliwka/vim-smoothie
Plug 'psliwka/vim-smoothie'

" Time (in milliseconds) between subseqent screen/cursor postion updates.
" Lower value produces smoother animation.
let g:smoothie_update_interval = 20

" Base scrolling speed (in lines per second), to be taken into account by
" the velocity calculation algorithm.  Can be decreased to achieve slower
" (and easier to follow) animation.
let g:smoothie_base_speed = 7
" -------------------------------------------------}}}

"                  Tmux integration                {{{
" ----------------------------------------------------
" Plug 'christoomey/vim-tmux-navigator'  " original
Plug 'anuvyklack/vim-tmux-navigator'  " my fork
" Activate autoupdate on exit
let g:tmux_navigator_save_on_switch = 0

" Disable vim->tmux navigation when the Vim pane is zoomed in tmux
let g:tmux_navigator_disable_when_zoomed = 1

" Plug 'tmux-plugins/vim-tmux-focus-events'
" Plug 'tmux-plugins/vim-tmux'
" -------------------------------------------------}}}

"              Syntaxes and languages              {{{
" ----------------------------------------------------
" This variable should be declared before polyglot is loaded!
let g:polyglot_disabled = ['markdown']
Plug 'sheerun/vim-polyglot'  " Подсветка синтаксисов разных языков

Plug 'lervag/vimtex', { 'for': 'LaTeX' }  " latex
Plug 'PProvost/vim-ps1', {'for': 'ps1'}   " powershell
Plug 'zinit-zsh/zinit-vim-syntax', { 'for': 'zsh' }  " zinit syntaxis
Plug 'anuvyklack/vim-dealii-prm', { 'for': 'prm' }
" Plug 'pearofducks/ansible-vim' ", { 'do': './UltiSnips/generate.sh' }
" -------------------------------------------------}}}

"           Русский язык (Switch language)         {{{
" ----------------------------------------------------
" Plug 'lyokha/vim-xkbswitch'
" let g:XkbSwitchEnabled = 1
" let g:XkbSwitchLib = '/mnt/c/tools/libxkbswitch64.dll'

Plug 'powerman/vim-plugin-ruscmd'
" -------------------------------------------------}}}

"                     Undotree                     {{{
" ----------------------------------------------------
Plug 'mbbill/undotree'         " visualize undo tree
" Plug 'simnalamburt/vim-mundo'  " another undo tree visualizer

let g:undotree_HighlightChangedWithSign = 0
let g:undotree_WindowLayout             = 2
" -------------------------------------------------}}}

"                   Color Schemes                  {{{
" ----------------------------------------------------
" Plug 'ayu-theme/ayu-vim'
" Plug 'joshdick/onedark.vim'
" Plug 'morhetz/gruvbox'
" Plug 'mhartington/oceanic-next'
" Plug 'ajmwagar/vim-deus'
Plug 'sainnhe/gruvbox-material'
" Plug 'habamax/vim-gruvbit'
Plug 'adigitoleo/vim-mellow'
" -------------------------------------------------}}}

call plug#end()

"                 ███                                    ██
"                ░░██                                   ░██
"   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
"  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
" ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
" ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
" ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
"  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░

" let s:theme = ''
" let s:theme = 'ayu'
" let s:theme = 'onedark'
" let s:theme = 'gruvbox'
let s:theme = 'gruvbox-material'
" let s:theme = 'gruvbit'
" let s:theme = 'mellow'
" let s:theme = 'gruvbox-8'
" let s:theme = 'deus'
" let s:theme = 'OceanicNext'

"              Color schemes settings              {{{
" ----------------------------------------------------

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
" }}}
" Gruvbit {{{
elseif s:theme == 'gruvbit'
    colorscheme gruvbit
    let g:airline_theme = 'gruvbox_material'
    if exists('g:lightline')
        let g:lightline.colorscheme = 'gruvbox_material'
    endif
" }}}
" Mellow {{{
elseif s:theme == 'mellow'
    set background=light
    " set background=dark
    colorscheme mellow
    let g:airline_theme = 'gruvbox_material'
    if exists('g:lightline')
        let g:lightline.colorscheme = 'gruvbox_material'
    endif
endif
" }}}

" -------------------------------------------------}}}

"                   ██     ██    ██
"                  ░██    ░██   ░░
"  ██████  █████  ██████ ██████ ███  ██████   ██████  ██████
" ██░░░░  ██░░░██░░░██░ ░░░██░ ░░██ ░██░░░██ ██░░░██ ██░░░░
"░░█████ ░███████  ░██    ░██   ░██ ░██  ░██░██  ░██░░█████
" ░░░░░██░██░░░░   ░██    ░██   ░██ ░██  ░██░░██████ ░░░░░██
" ██████ ░░█████   ░░███  ░░███ ░██ ░██  ░██ ░░░░░██ ██████
"░░░░░░   ░░░░░     ░░░    ░░░  ░░  ░░   ░░   █████ ░░░░░░
"                                            ░░░░░
" Here go settings that should be after the Vim-Plug call end.

"                     Treesitter                   {{{
" ----------------------------------------------------

" " Tree-sitter based folding:
" set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
" set foldnestmax=3

lua <<EOF
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  },

  -- incremental_selection = {
  --   enable = true,
  --   keymaps = {
  --     init_selection = "gnn",
  --     node_incremental = "grn",
  --     scope_incremental = "grc",
  --     node_decremental = "grm",
  --   },
  -- },

  -- textobjects = {
  --   enable = true,
  -- },

  indent = {
    enable = true,
  },

  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes
                     --   in the playground from source code.
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  }
}
EOF

" -------------------------------------------------}}}


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

"              Easymotion key bindings             {{{
" ----------------------------------------------------
map  ; <Plug>(easymotion-prefix)
nmap s <Plug>(easymotion-bd-f)
xmap s <Plug>(easymotion-bd-f)
omap s <Plug>(easymotion-bd-f)
nmap ;w <Plug>(easymotion-w)
nmap ;b <Plug>(easymotion-b)
nmap ;l <Plug>(easymotion-lineanywhere)
" -------------------------------------------------}}}

"                      Tmux                        {{{
" ----------------------------------------------------
let g:tmux_navigator_no_mappings = 1

" nnoremap <silent> <C-g><C-H> :TmuxNavigateLeft<cr>
" nnoremap <silent> <C-g><C-J> :TmuxNavigateDown<cr>
" nnoremap <silent> <C-g><C-K> :TmuxNavigateUp<cr>
" nnoremap <silent> <C-g><C-L> :TmuxNavigateRight<cr>

nnoremap <silent> <C-H> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-J> :TmuxNavigateDown<cr>
nnoremap <silent> <C-K> :TmuxNavigateUp<cr>
nnoremap <silent> <C-L> :TmuxNavigateRight<cr>

" nnoremap <silent> {Previous-Mapping} :TmuxNavigatePrevious<cr>
" -------------------------------------------------}}}

nnoremap <silent> gb :call ChooseBuffer()<CR>
function! ChooseBuffer() "{{{
    " Количество открытых буферов
    let num_of_buffers = len(getbufinfo({'buflisted':1}))
    if num_of_buffers > 2
        " If you are interesting what is <C-z> check ':help wildcharm'.
        " call feedkeys(":buffer \<C-z>")
        ToggleBufExplorer
    else
        bnext
    endif
endfunction "}}}

" " Показать syntax group для участка кода, а также цвет этой группы.
" " Удобно при создании своей цветовой схемы
" nnoremap <C-g> :call SyntaxAttr()<CR>


" vim: tw=76 cc=+1 fen fdm=marker
