--        ███                 ██                    ███
--       ░░██                ░░                    ░░██
-- ██████ ░██ ██   ██  ██████ ██ ██████   ██████    ░██ ██   ██  █████ 
--░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░     ░██░██  ░██ ░░░░░██
--░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████     ░██░██  ░██  ██████
--░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██    ░██░██  ░██ ██░░░██
--░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████  ██ ░██░░█████ ░░███████
--░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░  ░░  ░░  ░░░░░   ░░░░░░░ 
--░░                  ░░░░░

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
-- Performs `PackerClean` and then `PackerUpdate`.
-- :PackerSync
--
-- Loads opt plugin immediately.
-- :PackerLoad completion-nvim ale
---------------------------------------------------------

local fn = vim.fn
local execute = vim.api.nvim_command

-- Auto install packer.nvim if not exists
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty( fn.glob(install_path) ) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
end

-- -- This line is needed if packer.nvim is cloned in `opt` folder as suggested
-- in packer manual.  For more info look `:h packages`.
-- vim.cmd [[packadd packer.nvim]]

-- Auto compile when there are changes in plugins.lua
vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'


return require('packer').startup(function()

  use 'wbthomason/packer.nvim' -- Packer can manage itself
  
  -------------------- Text editing ---------------------
  
  use 'matze/vim-move'        -- перемещение строк и частей строк
  use 'wellle/targets.vim'    -- plugin that provides additional text objects
  use 'kshenoy/vim-signature' -- display and navigate marks
  use 'tpope/vim-unimpaired'  -- Different bidirectional motions: switch
                              --   buffers, add blank lines, etc.
  use 'tomtom/tcomment_vim'   -- comments

  -- Autopairs for neovim written by lua.
  -- https://github.com/windwp/nvim-autopairs
  use {
    'windwp/nvim-autopairs',
    config = function()
      require('nvim-autopairs').setup()
    end
  }

  use 'tpope/vim-surround'    -- заключать фрагменты текста в кавычки или скобки
  -- use {
  --   'machakann/vim-sandwich',
  --   config = function()
  --     vim.cmd("runtime macros/sandwich/keymap/surround.vim")
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

  ----------- Windows and buffers managment ------------
  use {
    'jlanzarotta/bufexplorer',
    config = function()
      -- Do not go to active window.
      vim.g.bufExplorerFindActive = 0
    end
  }

  use 'roxma/vim-window-resize-easy'

  -------------------- Movements ----------------------
  use {
    'easymotion/vim-easymotion',
    config = function() require('plugins_config.easymotion') end
  }

  -- -- Easymotion alternative
  -- use {
  --   'justinmk/vim-sneak',
  --   config = function()
  --     vim.g['sneak#label'] = 1
  --     vim.g['sneak#s_next'] = 1
  --   end
  -- }

  -- Smooth scroll                   
  -- https://github.com/psliwka/vim-smoothie
  use {
    'psliwka/vim-smoothie',
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

  -----------------------------------------------------

  ----------------- LSP and Completion ------------------

  -- For lsp the following has been working pretty well for me
  -- Nvim-lspconfig (for loading language servers)
  -- nvim-compe (for completion)
  -- lsp-trouble (for viewing info)
  -- And then come-tabnine as a tabnine source for compe + lspsaga for cool icons.
  -- Its definitely not a unified experience, but it isn't very hard to put
  -- together,

  -- use { 'neovim/nvim-lspconfig' }
  -- use { 'nvim-lua/completion-nvim' }

  -- use {
  --   'haorenW1025/completion-nvim',
  --   opt = true,
  --   requires = { {'hrsh7th/vim-vsnip', opt = true},
  --                {'hrsh7th/vim-vsnip-integ', opt = true} }
  -- }
  -------------------------------------------------------

  --------------------- Treesitter ----------------------
  use { 
    'nvim-treesitter/nvim-treesitter',
    requires = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'nvim-treesitter/nvim-treesitter-textobjects'
    },
    run = ':TSUpdate',
    config = function() require('plugins_config.treesitter') end
  }
  
  -------------------- Fuzzy finder ---------------------
  use {
    'nvim-telescope/telescope.nvim',
    requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'}
  }

  -------------------- IDE features ---------------------

  use 'tpope/vim-apathy'  -- Make 'gf' keybinding work in defferent filetypes.
                          -- For instructions of what it can do more look:
                          -- https://github.com/tpope/vim-apathy
                          -- and
                          -- :help include-search

  -- Asynchronous build and test dispatcher
  use {
    'tpope/vim-dispatch',
    opt = true,
    cmd = {'Dispatch', 'Make', 'Focus', 'Start'}
  }

  -- use {
  --   'lewis6991/gitsigns.nvim',
  --   requires = 'nvim-lua/plenary.nvim' ,
  --   config = function()
  --     require('gitsigns').setup()
  --   end
  -- }

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
  -- local theme = 'moonshine'
  -- local theme = 'srcery'

  if color_themes[theme] then
    color_themes[theme]()
  end
  -------------------------------------------------------

  -- --------------------- Statusline ----------------------
  -- use {
  --   'glepnir/galaxyline.nvim',
  --   branch = 'main',
  --   config = function() require'statusline' end,
  --   requires = {'kyazdani42/nvim-web-devicons'}
  -- }
  -- -------------------------------------------------------

  --                  Lua development                 {{{
  -------------------------------------------------------
  -- The library with all the lua functions you don't want to write twice.
  -- use { 'nvim-lua/plenary.nvim', opt = false }

  -- use { 'tjdevries/nlua.nvim' }

  -- use 'euclidianAce/BetterLua.vim' -- Better Lua syntax highlighting
  ----------------------------------------------------}}}

  --                 Tmux integration                {{{
  ------------------------------------------------------
  use {
    -- 'christoomey/vim-tmux-navigator',  -- original
    'anuvyklack/vim-tmux-navigator',  -- my fork

    config = function()
      -- Activate autoupdate on exit
      vim.g.tmux_navigator_save_on_switch = 0

      -- Disable vim->tmux navigation when the Vim pane is zoomed in tmux
      vim.g.tmux_navigator_disable_when_zoomed = 1
    end
  }

  use { 'tmux-plugins/vim-tmux-focus-events', disable=true }
  use { 'tmux-plugins/vim-tmux', disable=true }
  ---------------------------------------------------}}}
  
end)

-- vim: ts=2 sts=2 sw=2 tw=80 cc=+1 fen fdm=marker
