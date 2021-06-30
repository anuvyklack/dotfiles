--         ███                 ██                    ███
--        ░░██                ░░                    ░░██
--  ██████ ░██ ██   ██  ██████ ██ ██████   ██████    ░██ ██   ██  █████
-- ░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░     ░██░██  ░██ ░░░░░██
-- ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████     ░██░██  ░██  ██████
-- ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██    ░██░██  ░██ ██░░░██
-- ░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████  ██ ░██░░█████ ░░███████
-- ░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░  ░░  ░░  ░░░░░   ░░░░░░░
-- ░░                  ░░░░░

-- Packer instruction
---------------------------------------------------------
-- You must run this or `PackerSync` whenever you make
-- changes to your plugin configuration.
-- :PackerCompile
--
-- Only install missing plugins.
-- :PackerInstall
--
-- Update and install plugins.
-- :PackerUpdate
--
-- Remove any disabled or unused plugins.
-- :PackerClean
--
-- Performs `PackerClean` and then `PackerUpdate`.
-- :PackerSync
--
-- Loads opt plugin immediately.
-- :PackerLoad completion-nvim ale
---------------------------------------------------------

local fn = vim.fn
local execute = vim.api.nvim_command

-- Auto install packer.nvim if not exists
local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if fn.empty( fn.glob(install_path) ) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
end
vim.cmd [[packadd packer.nvim]]

-- -- Auto compile when there are changes in plugins.lua
-- -- vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'
-- vim.cmd [[
--   augroup Packer
--     autocmd!
--     autocmd BufWritePost plugins.lua PackerCompile
--   augroup end
-- ]]

require('packer').init{
  display = {
    open_cmd = '80vnew [packer]'  -- set the width of the packer split
  }
}

require('packer').startup(function(use)
-- return require('packer').startup(function()

  use { 'wbthomason/packer.nvim', opt = true } -- Packer can manage itself

  -------------------- Text editing ---------------------

  use 'matze/vim-move'          -- перемещение строк и частей строк
  use 'wellle/targets.vim'      -- plugin that provides additional text objects
  use 'kshenoy/vim-signature'   -- display and navigate marks
  use 'tpope/vim-unimpaired'    -- Different bidirectional motions: switch
                                --   buffers, add blank lines, etc.

  -- Comments {{{

  use {'tomtom/tcomment_vim',   -- Comments. For help use :help tcomment
    config = function() require('plugins_config/tcomment') end
  }

  -- use { 'winston0410/commented.nvim',
  --   config = function()
  --     require('commented').setup{}
  --   end
  -- }

  -- }}}

  -- Perl style regexp notation for Vim
  use { 'othree/eregex.vim', disable = true }

  -- Autopairs for neovim written by lua.
  -- https://github.com/windwp/nvim-autopairs
  use { 'windwp/nvim-autopairs',
    as = 'autopairs',
    config = function() require('nvim-autopairs').setup() end
  }

  -- Surround {{{

  -- Заключать фрагменты текста в кавычки или скобки.
  use { 'tpope/vim-surround',
    requires = 'tpope/vim-repeat'
  }

  -- -- Not yet as usable as tpope/vim-surround.
  -- use {
  --   "blackCauldron7/surround.nvim",
  --   config = function()
  --     vim.g.surround_mappings_style = 'surround'
  --     vim.g.surround_load_autogroups = true
  --
  --     require "surround".setup {}
  --   end
  -- }

  -- }}}

  -- improved * action
  use { 'haya14busa/vim-asterisk',
    as = 'asterisk',
    setup = function()
      vim.g['asterisk#keeppos'] = 1
    end
  }

  -- use { 'kevinhwang91/nvim-hlslens',
  --   as = 'hlslens',
  --   config = function() require('plugins_config/hlslens') end
  -- }

  -- Подсвечивать и удалять висящие пробелы в конце строк
  use { 'ntpeters/vim-better-whitespace',
    as = 'better-whitespace',
    config = function()
      vim.cmd('source ~/.config/nvim/lua/plugins_config/vim-better-whitespace.vim')
    end
  }

  -- Multiple cursors
  -- https://github.com/mg979/vim-visual-multi
  use 'mg979/vim-visual-multi'

  -------------------- Visual tweaks --------------------

  -- Подсвечивает все такие же слова как и слово под курсором.
  use { 'RRethy/vim-illuminate' }

  -- use 'itchyny/vim-cursorword'

  -- -- Найденный текст пульсирует
  -- use { 'inside/vim-search-pulse' }

  -- -- WARNING Work, but not very stable.
  -- -- indent-blankline {{{
  -- -- Adds indentation guides to all lines.
  -- use { 'lukas-reineke/indent-blankline.nvim',
  --   branch = 'lua',
  --   ft = {'lua', 'python'},
  --   config = function()
  --     -- Filetypes for which this plugin is enabled
  --     vim.g.indent_blankline_filetype = {'lua', 'python'}
  --
  --     vim.g.indent_blankline_char = '│'
  --     vim.g.indent_blankline_use_treesitter = true
  --     vim.g.indent_blankline_show_first_indent_level = false
  --     vim.g.indent_blankline_show_trailing_blankline_indent = false
  --
  --     vim.g.indent_blankline_show_current_context = true
  --     vim.g.indent_blankline_context_highlight_list = {'Error', 'Warning'}
  --   end
  -- } --}}}

  ------------ Windows and buffers managment ------------

  -- use 'matbme/JABS.nvim'

  use { 'jlanzarotta/bufexplorer',
    config = function()
      -- Do not go to active window.
      vim.g.bufExplorerFindActive = 0
    end
  }

  use 'roxma/vim-window-resize-easy'

  ---------------------- Movements ----------------------

  -- use { 'easymotion/vim-easymotion',
  --   as = 'easymotion',
  --   config = function() require('plugins_config/easymotion') end
  -- }

  use { 'phaazon/hop.nvim',
    as = 'easymotion_hop',
    config = function()
      require'hop'.setup {
        winblend = 30
      }
      require('keybindings').hop()
    end
  }

  -- Smooth scroll {{{

  -- https://github.com/psliwka/vim-smoothie
  use { 'psliwka/vim-smoothie',
    config = function()
      -- Time (in milliseconds) between subseqent screen/cursor postion updates.
      -- Lower value produces smoother animation.
      vim.g.smoothie_update_interval = 20
      --
      -- Base scrolling speed (in lines per second), to be taken into account by
      -- the velocity calculation algorithm.  Can be decreased to achieve slower
      -- (and easier to follow) animation.
      vim.g.smoothie_base_speed = 7
    end
  }

  -- use { 'karb94/neoscroll.nvim',
  --   config = function() require('plugins_config/neoscroll') end
  -- }

  -- }}}

  -- -- Clever-f
  -- use { 'rhysd/clever-f.vim',
  --   config = function()
  --     vim.g.clever_f_ignore_case = 1
  --     vim.g.clever_f_smart_case = 1
  --     vim.g.clever_f_show_prompt = 1
  --     vim.g.clever_f_chars_match_any_signs = ';'
  --   end
  -- }

  -- use 'chaoren/vim-wordmotion'  -- More useful word motions for Vim

  -------------------------------------------------------

  --                LSP and Completion                {{{
  -------------------------------------------------------

  -- For lsp the following has been working pretty well for me
  -- nvim-lspconfig (for loading language servers)
  -- nvim-compe (for completion)
  -- lsp-trouble (for viewing info)
  -- And then come-tabnine as a tabnine source for compe + lspsaga for cool icons.
  -- Its definitely not a unified experience, but it isn't very hard to put
  -- together,

  use { 'neovim/nvim-lspconfig',
    as = 'lspconfig',
    requires = {
      -- Adds the missing :LspInstall <language> command
      -- to conveniently install language servers.
      { "kabouzeid/nvim-lspinstall", as = 'lspinstall'},
      -- Setup for lua and plugins development.
      "folke/lua-dev.nvim"
    },
    config = function() require('plugins_config/lspconfig') end
    -- config = function() require('plugins_config/lspconfig-test') end
  }

  use { 'glepnir/lspsaga.nvim',
  -- use { "jasonrhansen/lspsaga.nvim",
    -- branch = 'finder-preview-fixes',
    config = function() require('plugins_config/lspsaga') end
  }

  -- use { "folke/trouble.nvim",
  --   requires = "kyazdani42/nvim-web-devicons",
  --   config = function() require('plugins_config/trouble') end
  -- }

  -- use 'nvim-lua/diagnostic-nvim'

  -- Completion {{{

  -- use { 'nvim-lua/completion-nvim' }  -- has performance issues
  use { 'hrsh7th/nvim-compe',
    config = function() require('plugins_config/nvim-compe') end
  }

  -- Icons in completion menu.
  use { "onsails/lspkind-nvim",
    config = function() require('plugins_config/lspkind-nvim') end
  }

  -- use { 'haorenW1025/completion-nvim',
  --   opt = true,
  --   requires = { {'hrsh7th/vim-vsnip', opt = true},
  --                {'hrsh7th/vim-vsnip-integ', opt = true} }
  -- }

  -- }}}

  -- Lsp signature hint when you type.
  use { "ray-x/lsp_signature.nvim",
    config = function()
      require('lsp_signature').on_attach()
    end
  }

  use { 'jubnzv/virtual-types.nvim' }

  use { 'simrat39/symbols-outline.nvim' }
  -- use { "liuchengxu/vista.vim" }

  use { "ahmedkhalf/lsp-rooter.nvim",
    config = function() require("lsp-rooter").setup() end
  }

  -- use 'airblade/vim-rooter'

  -- use 'nvim-lua/lsp_extensions.nvim'
  -- use 'steelsojka/completion-buffers'
  -- use 'tjdevries/nlua.nvim'


  ----------------------------------------------------}}}

  --           DAP (Debug Adapter Protocol)           {{{
  -------------------------------------------------------
  use { "rcarriga/nvim-dap-ui",
    requires = "mfussenegger/nvim-dap"
  }
  ----------------------------------------------------}}}

  --------------------- Treesitter ----------------------
  use { 'nvim-treesitter/nvim-treesitter',
    as = 'treesitter',
    requires = {
      {'nvim-treesitter/nvim-treesitter-refactor',    as = 'treesitter-refactor'},
      {'nvim-treesitter/nvim-treesitter-textobjects', as = 'treesitter-textobjects'},
      {'RRethy/nvim-treesitter-textsubjects',         as = 'treesitter-textsubjects'},
      {'nvim-treesitter/playground',                  as = 'treesitter-playground'},
      {'p00f/nvim-ts-rainbow',                        as = 'treesitter-rainbow'},
      {'romgrk/nvim-treesitter-context',              as = 'treesitter-context'},
    },
    run = ':TSUpdate',
    config = function() require('plugins_config/treesitter') end
  }

  -------------------- Fuzzy finder ---------------------
  use { 'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}
  }

  -------------------- IDE features ---------------------

  use 'tpope/vim-apathy'  -- Make 'gf' keybinding work in different filetypes.
                          -- For instructions of what it can do more look:
                          -- https://github.com/tpope/vim-apathy
                          -- and
                          -- :help include-search

  -- use 'wellle/context.vim'   -- Vscode breadcrumbs analog
  -- use 'majutsushi/tagbar'    -- список тегов в текущем файле
  -- use 'liuchengxu/vista.vim' -- View and search LSP symbols and tags.

  -- Asynchronous build and test dispatcher
  use { 'tpope/vim-dispatch',
    cmd = {'Dispatch', 'Make', 'Focus', 'Start'}
  }

  -- use 'junegunn/gv.vim'  -- A git commit browser in Vim.

  -- use { 'lewis6991/gitsigns.nvim',
  --   requires = 'nvim-lua/plenary.nvim' ,
  --   config = function()
  --     require('gitsigns').setup()
  --   end
  -- }

  ---------- Syntaxes and programming languages ----------

  -- Подсветка синтаксисов для разных языков.
  use { 'sheerun/vim-polyglot',
    setup = function()  -- run before plugin load
      -- This variable should be declared before polyglot is loaded!
      -- vim.g.polyglot_disabled = 'markdown'
      vim.g.polyglot_disabled = {'markdown', 'gitignore'}
    end
  }

  use 'SirJson/fzf-gitignore'

  use 'Neui/cmakecache-syntax.vim'
  -- use { 'zinit-zsh/zinit-vim-syntax', ft = 'zsh' } -- zinit syntaxis
  use { 'anuvyklack/vim-dealii-prm', event = 'BufNewFile,BufRead *.prm' }

  -------------------- Color scheme ---------------------
  local color_themes = {
    -- gruvbox-material {{{
    ['gruvbox-material'] = function()
    -- ['gruvbox-material'] = function()
      use {
        'sainnhe/gruvbox-material',
        config = function()
          vim.o.background = 'dark'
          -- vim.o.background = 'light'

          -- Set the color palette used in this color scheme.
          -- material : material palette with soft contrast;
          -- mix      : the mean of the other two;
          -- original : the original gruvbox palette.
          vim.g.gruvbox_material_palette = 'mix'

          -- Set contrast.
          -- available values: 'hard', 'medium'(default), 'soft'
          vim.g.gruvbox_material_background = 'medium'
          vim.g.gruvbox_material_enable_bold = 1
          vim.g.gruvbox_material_enable_italic = 1

          -- Available values: 'auto', 'red', 'orange', 'yellow',
          -- 'green', 'aqua', 'blue', 'purple'
          vim.g.gruvbox_material_cursor = 'blue'

          vim.cmd 'colorscheme gruvbox-material'
          vim.cmd('source ~/.config/nvim/lua/plugins_config/gruvbox-material.vim')
        end
      }
    end, --}}}
    -- melange {{{
    ['melange'] = function()
      use {
        'savq/melange',
        config = function()
          -- vim.o.background = 'light'
          vim.cmd 'colorscheme melange'
        end
      }
    end, --}}}
    -- moonshine {{{
    ['moonshine'] = function()
      use {
        'karoliskoncevicius/moonshine-vim',
        config = function()
          vim.cmd 'colorscheme moonshine'
          -- vim.cmd 'colorscheme moonshine_lowcontrast'
          -- vim.cmd 'colorscheme moonshine_minimal'
        end
      }
    end, --}}}
    -- srcery {{{
    ['srcery'] = function()
      use { 'srcery-colors/srcery-vim' }
    end, --}}}
    -- mellow {{{
    ['mellow'] = function()
      use {
        'adigitoleo/vim-mellow',
        as = 'mellow',
        config = function()
          vim.o.background = 'light'
          -- vim.o.background = 'dark'
          vim.cmd 'colorscheme mellow'
          vim.cmd('source ~/.config/nvim/lua/plugins_config/mellow.vim')
        end
      }
    end, --}}}
    -- tokyonight {{{
    ['tokyonight'] = function()
      use {
        'folke/tokyonight.nvim',
        setup = function()
          vim.g.tokyonight_style = "storm"
        end,
        config = function()
          vim.cmd 'colorscheme tokyonight'
        end
      }
    end, --}}}
  }

  local theme = 'gruvbox-material'
  -- local theme = 'tokyonight'
  -- local theme = 'melange'
  -- local theme = 'mellow'
  -- local theme = 'moonshine'
  -- local theme = 'srcery'

  if color_themes[theme] then
    color_themes[theme]()
  end
  -------------------------------------------------------

  -- --------------------- Statusline ----------------------
  -- use { 'glepnir/galaxyline.nvim',
  --   branch = 'main',
  --   config = function() require'statusline' end,
  --   requires = {'kyazdani42/nvim-web-devicons'}
  -- }
  -- -------------------------------------------------------

  --------------- Vim additional modules ----------------

  -- bufferline / tabline

  use { 'romgrk/barbar.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      -- vim.cmd('source ~/.config/nvim/lua/plugins_config/barbar.vim')
      require('plugins_config/barbar')
    end
  }

  -- use { 'akinsho/nvim-bufferline.lua',
  --   requires = 'kyazdani42/nvim-web-devicons',
  --   config = function() require('plugins_config/nvim-bufferline') end
  -- }

  -- -- visualize undo tree
  -- use { 'mbbill/undotree',
  --   config = function()
  --     vim.g.undotree_HighlightChangedWithSign = 0
  --     vim.g.undotree_WindowLayout = 2
  --   end
  -- }

  -- -- another undo tree visualizer
  -- use 'simnalamburt/vim-mundo'

  use { "folke/which-key.nvim",
    config = function()
      require("which-key").setup {
        spelling = {
          enabled = true,  -- Enabling this will show WhichKey when pressing z=
                           --   to select spelling suggestions.
          suggestions = 20, -- How many suggestions should be shown in the list?
        },
      }
    end
  }

  -- <leader>? : 40-column cheat sheet
  use 'lifepillar/vim-cheat40'

  -- TODO Need to rewrite in lua.
  -- use { 'camspiers/lens.vim',
  --   requires = 'camspiers/animate.vim',
  --   config = function()
  --     vim.cmd('source ~/.config/nvim/lua/plugins_config/lens.vim')
  --   end
  -- }

  -- -- foldtext customization
  -- use 'scr1pt0r/crease.vim'

  use { 'norcalli/nvim-colorizer.lua',
    as = 'colorizer',
    ft = {'vim', 'lua', 'conf'},
    config = function()
      require'colorizer'.setup()
    end
  }

  -------------------------------------------------------

  -------------------- File manager ---------------------

  use { 'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require('plugins_config/nvim-tree') end
  }

  -- use { 'ms-jpq/chadtree',
  --   branch = 'chad',
  --   run = 'python3 -m chadtree deps && nvim --headless +"CHADdeps | exit"',
  --   cmd = {'CHADopen', 'CHADdeps', 'CHADhelp'}
  -- }

  -- -- Total Commander inspired file manager
  -- use { 'ripxorip/bolt.nvim',
  --   run = ':UpdateRemotePlugins'
  -- }

  -------------------------------------------------------

  -- --                  Lua development                 {{{
  -- -----------------------------------------------------
  --
  -- use { 'tjdevries/nlua.nvim' }
  --
  -- use 'euclidianAce/BetterLua.vim' -- Better Lua syntax highlighting

  ----------------------------------------------------}}}

  --                       Vifm                       {{{
  -------------------------------------------------------
  use { "vifm/vifm.vim",
    -- TODO Open issue to packer: help tags not generated automaticaly.
    run = ':helptags ALL',
  }
  ----------------------------------------------------}}}

  --                 Tmux integration                {{{
  ------------------------------------------------------
  use {
    'anuvyklack/vim-tmux-navigator',  -- my fork
    -- 'christoomey/vim-tmux-navigator',  -- original

    config = function()
      -- Activate autoupdate on exit.
      vim.g.tmux_navigator_save_on_switch = 0

      -- Disable vim->tmux navigation when the Vim pane is zoomed in tmux.
      vim.g.tmux_navigator_disable_when_zoomed = 1
    end
  }

  use { 'tmux-plugins/vim-tmux-focus-events', disable=true }
  use { 'tmux-plugins/vim-tmux', disable=true }
  ---------------------------------------------------}}}

  --                      Pandoc                      {{{
  -------------------------------------------------------
  use { 'vim-pandoc/vim-pandoc',
    ft = {'markdown', 'pandoc'},
    requires = {'vim-pandoc/vim-pandoc-syntax', opt = true},
    config = function()
      vim.cmd('source ~/.config/nvim/lua/plugins_config/pandoc.vim')
    end
  }
  ----------------------------------------------------}}}

  --          Русский язык (Switch language)          {{{
  -------------------------------------------------------

  -- use { 'lyokha/vim-xkbswitch',
  --   config = function()
  --     vim.g.XkbSwitchEnabled = 1
  --   end
  -- }

  use 'powerman/vim-plugin-ruscmd'

  ----------------------------------------------------}}}

end)

-- vim: ts=2 sts=2 sw=2 tw=80 cc=+1 fen fdm=marker
