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

-- Auto compile when there are changes in plugins.lua
-- vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'
vim.cmd [[
  augroup Packer
    autocmd!
    autocmd BufWritePost plugins.lua PackerCompile
  augroup end
]]

require('packer').init{
  display = {
    open_cmd = '80vnew [packer]'  -- set the width of the packer split
  }
}

require('packer').startup(function()
-- return require('packer').startup(function()

  use { 'wbthomason/packer.nvim', opt = true } -- Packer can manage itself

  -------------------- Text editing ---------------------

  use 'matze/vim-move'        -- перемещение строк и частей строк
  use 'wellle/targets.vim'    -- plugin that provides additional text objects
  use 'kshenoy/vim-signature' -- display and navigate marks
  use 'tpope/vim-unimpaired'  -- Different bidirectional motions: switch
                              --   buffers, add blank lines, etc.
  use 'tomtom/tcomment_vim'   -- Comments. For help use :help tcomment
  
  -- Perl style regexp notation for Vim
  use { 'othree/eregex.vim', disable = true }
  
  -- Autopairs for neovim written by lua.
  -- https://github.com/windwp/nvim-autopairs
  use { 'windwp/nvim-autopairs',
    config = function()
      require('nvim-autopairs').setup()
    end
  }
  
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
  
  -- -- Подсвечивать и удалять висящие пробелы в конце строк
  -- use { 'ntpeters/vim-better-whitespace',
  --   config = function()
  --     vim.cmd('source ~/.config/nvim/lua/plugins_config/vim-better-whitespace.vim')
  --   end
  -- }
  
  -- Multiple cursors
  -- https://github.com/mg979/vim-visual-multi
  use 'mg979/vim-visual-multi'
  
  -------------------- Visual tweaks --------------------
  
  -- Подсвечивает все такие же слова как и слово под курсором.
  use { 'RRethy/vim-illuminate', disable=true }
   
  -- Найденный текст пульсирует
  use { 'inside/vim-search-pulse', disable=true }
  
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
        
  -- use 'kyazdani42/nvim-web-devicons'
  
  ------------ Windows and buffers managment ------------
  
  use { 'jlanzarotta/bufexplorer',
    config = function()
      -- Do not go to active window.
      vim.g.bufExplorerFindActive = 0
    end
  }
  
  use 'roxma/vim-window-resize-easy'
  
  ---------------------- Movements ----------------------
  
  use { 'easymotion/vim-easymotion',
    config = function() require('plugins_config.easymotion') end
  }
  
  -- Smooth scroll                   
  -- https://github.com/psliwka/vim-smoothie
  use { 'psliwka/vim-smoothie',
    config = function()
      -- Time (in milliseconds) between subseqent screen/cursor postion updates.
      -- Lower value produces smoother animation.
      vim.g.smoothie_update_interval = 20
  
      -- Base scrolling speed (in lines per second), to be taken into account by
      -- the velocity calculation algorithm.  Can be decreased to achieve slower
      -- (and easier to follow) animation.
      vim.g.smoothie_base_speed = 7
    end
  }
  
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
  
  -- You're probably better-served using compile_commands.json which can be
  -- autogenerated in multiple ways.
  --
  -- If you use CMake, you can '-D CMAKE_EXPORT_COMPILE_COMMANDS=1' when
  -- generating build files to generate the compile_commands.json, or if you use
  -- regular Make, there's a tool called bear that'll hook onto make to generate
  -- it for you.
  
  use { 'neovim/nvim-lspconfig',
    config = function() require('plugins_config.lspconfig') end
  }
  
  -- -- Adds the missing :LspInstall <language> command
  -- -- to conveniently install language servers.
  -- use 'kabouzeid/nvim-lspinstall'

  -- use { 'RishabhRD/nvim-lsputils',
  --   requires = 'RishabhRD/popfix',
  --   config = function()
  --     vim.lsp.handlers['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
  --     vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
  --     vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
  --     vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
  --     vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
  --     vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
  --     vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
  --     vim.lsp.handlers['workspace/symbol'] = require'lsputil.symbols'.workspace_handler
  --   end
  -- } 

  use { 'glepnir/lspsaga.nvim',
    config = function()
      require('plugins_config.lspsaga')
    end
  }

  -- use 'nvim-lua/diagnostic-nvim'

  -- use { 'nvim-lua/completion-nvim' }
  
  -- use { 'haorenW1025/completion-nvim',
  --   opt = true,
  --   requires = { {'hrsh7th/vim-vsnip', opt = true},
  --                {'hrsh7th/vim-vsnip-integ', opt = true} }
  -- }

  -- use 'nvim-lua/lsp_extensions.nvim'
  -- use 'steelsojka/completion-buffers'
  -- use 'tjdevries/nlua.nvim'
  

  ----------------------------------------------------}}}
  
  --------------------- Treesitter ----------------------
  use { 'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'nvim-treesitter/nvim-treesitter-textobjects',
      'nvim-treesitter/playground'
    },
    run = ':TSUpdate',
    config = function() require('plugins_config.treesitter') end
  }
  
  -------------------- Fuzzy finder ---------------------
  use { 'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}
  }
  
  -------------------- IDE features ---------------------
  
  use 'tpope/vim-apathy'  -- Make 'gf' keybinding work in defferent filetypes.
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

  use { 'zinit-zsh/zinit-vim-syntax', ft = 'zsh' } -- zinit syntaxis
  use { 'anuvyklack/vim-dealii-prm', event = 'BufNewFile,BufRead *.prm' }
  
  -------------------- Color scheme ---------------------
  local color_themes = {
    -- gruvbox-material {{{
    ['gruvbox-material'] = function()
      use {
        'sainnhe/gruvbox-material',
        config = function()
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
        config = function()
          vim.o.background = 'light'
          -- vim.o.background = 'dark'
          vim.cmd 'colorscheme mellow'
        end
      }
    end, --}}}
  }
  
  local theme = 'gruvbox-material'
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
      vim.cmd('source ~/.config/nvim/lua/plugins_config/barbar.vim')
    end
  }
  
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
        -- Your configuration comes here
        -- or leave it empty to use the default settings.
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
    ft = {'vim', 'lua', 'conf'},
    config = function()
      require'colorizer'.setup()
    end
  }

  -------------------------------------------------------
  
  -------------------- File manager ---------------------
  
  use { 'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      vim.cmd('source ~/.config/nvim/lua/plugins_config/nvim-tree.vim')
    end
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

  --                  Lua development                 {{{
  -------------------------------------------------------
  -- The library with all the lua functions you don't want to write twice.
  -- use { 'nvim-lua/plenary.nvim', opt = false }

  -- use { 'tjdevries/nlua.nvim' }

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