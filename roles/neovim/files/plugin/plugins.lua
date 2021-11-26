--         ███                 ██                    ███
--        ░░██                ░░                    ░░██
--  ██████ ░██ ██   ██  ██████ ██ ██████   ██████    ░██ ██   ██  █████
-- ░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░     ░██░██  ░██ ░░░░░██
-- ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████     ░██░██  ░██  ██████
-- ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██    ░██░██  ░██ ██░░░██
-- ░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████  ██ ░██░░█████ ░░███████
-- ░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░  ░░  ░░  ░░░░░   ░░░░░░░
-- ░░                  ░░░░░

-- -------------- ------------------------------------------------
-- :PackerCompile | You must run this or `PackerSync` whenever you
--                  make changes to your plugin configuration.
-- :PackerInstall | Only install missing plugins.
-- :PackerUpdate  | Update and install plugins.
-- :PackerClean   | Remove any disabled or unused plugins.
-- :PackerSync    | Performs `PackerClean` then `PackerUpdate`
--                  and `PackerCompile` at the end.
-- -------------- ------------------------------------------------

local fn = vim.fn
local execute = vim.api.nvim_command

-- setup Packer {{{
-- Auto install packer.nvim if not exists
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
   fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', '--depth', '1', install_path})
   execute 'packadd packer.nvim'
end

-- -- Auto compile when there are changes in plugins.lua
-- -- vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'
-- vim.cmd [[
--   augroup Packer
--     autocmd!
--     autocmd BufWritePost plugins.lua PackerCompile
--   augroup end
-- ]]

-- Packer settings
require('packer').init{
   display = {
      -- open_cmd = '80vnew [packer]',  -- set the width of the packer split
      open_fn = function()
         return require("packer.util").float({border = "rounded"})
         -- return require("packer.util").float()
      end,
      working_sym = '', --      plugin being installed/updated
      error_sym = '',   -- plugin with an error in installation/updating
      done_sym = '',    --   plugin which has completed installation/updating
      --       ﯇ 
      --  
   },
   git = {
      clone_timeout = 600  -- timeout, in seconds, for git clones
   }
}
-- }}}

return require('packer').startup(function()
   use 'wbthomason/packer.nvim' -- Packer can manage itself

   -------------------- Text editing ---------------------

   use 'matze/vim-move'         -- перемещение строк и частей строк
   use 'wellle/targets.vim'     -- plugin that provides additional text objects
   use 'kshenoy/vim-signature'  -- display and navigate marks
   use 'tpope/vim-unimpaired'   -- Different bidirectional motions: switch
                                --   buffers, add blank lines, etc.

   -- Until https://github.com/neovim/neovim/pull/13823 will be merged.
   use 'tjdevries/astronauta.nvim'

   -- Comments {{{

   use { 'tomtom/tcomment_vim',   -- Comments. For help use :help tcomment
      as = 'tcomment',
      -- config = function() require('plugins_config/tcomment') end
   }

   -- use { 'winston0410/commented.nvim',
   --   config = function()
   --     require('commented').setup{}
   --   end
   -- }

   -- }}}

   -- -- Perl style regexp notation for Vim
   -- use  'othree/eregex.vim'

   -- Autopairs {{{
   use { 'windwp/nvim-autopairs',
      as = 'autopairs',
      config = function()
         require('nvim-autopairs').setup()
         -- require("nvim-autopairs.completion.compe").setup({
         --    map_cr = true, -- map <CR> on insert mode
         --    map_complete = true -- It will auto insert `(` after select function or method item.
         -- })
      end
   }
   -- }}}

   -- Surround {{{

   -- Заключать фрагменты текста в кавычки или скобки.
   use { 'tpope/vim-surround',
      requires = 'tpope/vim-repeat'
   }

   -- -- Not yet as usable as tpope/vim-surround.
   -- use {
   --    "blackCauldron7/surround.nvim",
   --    config = function()
   --       vim.g.surround_mappings_style = 'surround'
   --       vim.g.surround_load_autogroups = true
   --
   --       require "surround".setup {}
   --    end
   -- }

   -- }}}

   -- Improved * keybinding action.
   use { 'haya14busa/vim-asterisk',
      as = 'asterisk',
      setup = function()
         vim.g['asterisk#keeppos'] = 1
      end
   }

   -- use { 'kevinhwang91/nvim-hlslens',
   --    as = 'hlslens',
   --    config = function() require('plugins_config/hlslens') end
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
   use { 'mg979/vim-visual-multi', as = 'multiple-cursors'}

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
   --    branch = 'lua',
   --    ft = {'lua', 'python'},
   --    config = function()
   --       -- Filetypes for which this plugin is enabled
   --       vim.g.indent_blankline_filetype = {'lua', 'python'}
   --
   --       vim.g.indent_blankline_char = '│'
   --       vim.g.indent_blankline_use_treesitter = true
   --       vim.g.indent_blankline_show_first_indent_level = false
   --       vim.g.indent_blankline_show_trailing_blankline_indent = false
   --
   --       vim.g.indent_blankline_show_current_context = true
   --       vim.g.indent_blankline_context_highlight_list = {'Error', 'Warning'}
   --    end
   -- } --}}}

   ------------ Windows and buffers managment ------------

   -- use 'matbme/JABS.nvim'

   use { 'jlanzarotta/bufexplorer',
      config = function()
         vim.g.bufExplorerFindActive = 0 -- Do not go to active window.
      end
   }

   use { 'roxma/vim-window-resize-easy', as = 'window-resize-easy' }

   -- use { 'simeji/winresizer',
   --   config = function()
   --     vim.g.winresizer_vert_resize  = 1
   --     vim.g.winresizer_horiz_resize = 1
   --     vim.g.winresizer_start_key = '<leader>w'
   --
   --     -- require("util").set_keymap('n', '<leader>w', 'Window resize mode',
   --     --                            '<cmd>WinResizerStartResize<CR>',
   --     --                            {noremap = true, silent = true})
   --   end
   -- }

   -- use 'zhaocai/GoldenView.Vim'
   use {'RobertAudi/GoldenView.vim', opt = true}

   ---------------------- Movements ----------------------

   -- use { 'easymotion/vim-easymotion',
   --   as = 'easymotion',
   --   config = function() require('plugins_config/easymotion') end
   -- }

   use { 'phaazon/hop.nvim',
      as = 'easymotion-hop',
      config = function()
         require'hop'.setup {
            winblend = 30,
            -- keys = 'asdghklqwertyuiopzxcvbnmfj'
            -- keys = 'asdghklqwertyuiopzxcvfjbnm'
            keys = 'asdghklqwertyuiopzxcvfjbn'
         }
         require('keybindings').hop()
      end
   }

   -- use {'ggandor/lightspeed.nvim', }

   --                   Smooth scroll                  {{{
   -------------------------------------------------------

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
   --    config = function() require('plugins_config/neoscroll') end
   -- }

   ----------------------------------------------------}}}

   -- -- Clever-f
   -- use { 'rhysd/clever-f.vim',
   --    config = function()
   --       vim.g.clever_f_ignore_case = 1
   --       vim.g.clever_f_smart_case = 1
   --       vim.g.clever_f_show_prompt = 1
   --       vim.g.clever_f_chars_match_any_signs = ';'
   --    end
   -- }

   -- use 'chaoren/vim-wordmotion'  -- More useful word motions for Vim

   -------------------------------------------------------

   --             LSP, Completion, Snippets            {{{
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
         {'kabouzeid/nvim-lspinstall',
            as = 'lspinstall'},

         -- Setup for lua and plugins development.
         {'folke/lua-dev.nvim',
            as = 'lua-dev'}
      },
      config = function() require('plugins_config/lspconfig') end
   }

   -- use { 'glepnir/lspsaga.nvim',
   -- -- use { "jasonrhansen/lspsaga.nvim",
   --    -- branch = 'finder-preview-fixes',
   --    as = 'lspsaga',
   --    config = function() require('plugins_config/lspsaga') end
   -- }

   use { 'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
      cmd = {'Trouble', 'TroubleToggle'},
      config = function() require('plugins_config/trouble') end
   }

   -- use 'nvim-lua/diagnostic-nvim'

   -- Snippets {{{

   -- https://github.com/L3MON4D3/LuaSnip
   -- https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md
   -- https://github.com/L3MON4D3/LuaSnip/wiki/Cool-Snippets#all---pairs
   -- https://github.com/L3MON4D3/LuaSnip/wiki/Nice-Configs
   use { 'L3MON4D3/LuaSnip' }

   -- The community driven  collection of snippets.
   use { 'rafamadriz/friendly-snippets' }

   -- }}}

   -- Completion {{{

   -- Icons in completion menu.
   use { 'onsails/lspkind-nvim',
      as = 'lspkind',
      config = function() require('plugins_config/lspkind-nvim') end
   }

   use { 'hrsh7th/nvim-cmp',
      requires = {
         'hrsh7th/cmp-nvim-lsp',
         'hrsh7th/cmp-buffer'
      },
      config = function() require('plugins_config/nvim-cmp') end
   }

   -- }}}

   -- Lsp signature hint when you type.
   use { 'ray-x/lsp_signature.nvim', as = 'lsp-signature' }

   use { 'jubnzv/virtual-types.nvim', as = 'virtual-types' }

   use { 'simrat39/symbols-outline.nvim',
      as = 'symbols-outline',
      config = function() require('plugins_config/symbols-outline') end
   }

   -- use { "liuchengxu/vista.vim" }

   use { 'ahmedkhalf/lsp-rooter.nvim',
      as = 'lsp-rooter',
      config = function() require('lsp-rooter').setup() end
   }

   -- use 'airblade/vim-rooter'

   -- use 'nvim-lua/lsp_extensions.nvim'
   -- use 'steelsojka/completion-buffers'
   -- use 'tjdevries/nlua.nvim'


   ----------------------------------------------------}}}

   -- --           DAP (Debug Adapter Protocol)           {{{
   -- -------------------------------------------------------
   -- use { "mfussenegger/nvim-dap", as = 'dap' }
   -- use { "rcarriga/nvim-dap-ui",  as = 'dap-ui' }
   -- ----------------------------------------------------}}}

   --------------------- Treesitter ----------------------
   use { 'nvim-treesitter/nvim-treesitter',
      as = 'treesitter',
      requires = {
         {'nvim-treesitter/nvim-treesitter-refactor',
            as = 'treesitter-refactor'},
         {'nvim-treesitter/nvim-treesitter-textobjects',
            as = 'treesitter-textobjects'},
         {'RRethy/nvim-treesitter-textsubjects',
            as = 'treesitter-textsubjects'},
         {'nvim-treesitter/playground',
            as = 'treesitter-playground'},
         {'p00f/nvim-ts-rainbow',
            as = 'treesitter-rainbow'},
         {'romgrk/nvim-treesitter-context',
            as = 'treesitter-context'},
      },
      run = ':TSUpdate',
      config = function() require('plugins_config/treesitter') end
   }

   -------------------- Fuzzy finder ---------------------
   use { 'nvim-telescope/telescope.nvim',
      as = 'telescope',
      requires = {
         'nvim-lua/popup.nvim',
         'nvim-lua/plenary.nvim',
         -- 'famiu/bufdelete.nvim',
         'jvgrootveld/telescope-zoxide'
      },
      config = function() require('plugins_config/telescope') end,
      cmd = 'Telescope',
      keys = {'<leader>f'}
   }

   -------------------- IDE features ---------------------

   -- use 'tpope/vim-apathy'  -- Make 'gf' keybinding work in different filetypes.
   --                         -- For instructions of what it can do more look:
   --                         -- https://github.com/tpope/vim-apathy
   --                         -- and
   --                         -- :help include-search

   -- use 'wellle/context.vim'   -- Vscode breadcrumbs analog
   -- use 'majutsushi/tagbar'    -- список тегов в текущем файле
   -- use 'liuchengxu/vista.vim' -- View and search LSP symbols and tags.

   -- Asynchronous build and test dispatcher
   use { 'tpope/vim-dispatch',
      cmd = {'Dispatch', 'Make', 'Focus', 'Start'}
   }

   -- use 'junegunn/gv.vim'  -- A git commit browser in Vim.

   -- use { 'lewis6991/gitsigns.nvim',
   --    requires = 'nvim-lua/plenary.nvim' ,
   --    config = function()
   --       require('gitsigns').setup()
   --    end
   -- }

   use { 'TimUntersberger/neogit',
      requires = 'nvim-lua/plenary.nvim'
   }

   --------- Syntaxes and programming languages ----------

   -- Подсветка синтаксисов для разных языков.
   use { 'sheerun/vim-polyglot',
      setup = function()  -- run before plugin load
         -- This variable should be declared before polyglot is loaded!
         -- vim.g.polyglot_disabled = 'markdown'
         vim.g.polyglot_disabled = {'markdown', 'gitignore', 'txt'}
      end
   }

   use 'SirJson/fzf-gitignore'

   use { 'Neui/cmakecache-syntax.vim', as = 'syntax-cmakecache'}
   -- use { 'zinit-zsh/zinit-vim-syntax', ft = 'zsh' } -- zinit syntaxis

   use { 'anuvyklack/vim-dealii-prm',
      as = 'syntax-dealii-prm',
      -- event = 'BufNewFile,BufRead *.prm'
   }

   -- Kitty terminal conf file syntax highlight.
   use { "fladson/vim-kitty", as = 'syntax-kitty-conf'}

   -------------------------------------------------------

   --                   Color scheme                   {{{
   -------------------------------------------------------
   local color_themes = {
      -- gruvbox-material-dark {{{
      ['gruvbox-material-dark'] = function()
         use {
            'sainnhe/gruvbox-material',
            after = 'treesitter',
            config = function()
               vim.o.background = 'dark'

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
               -- vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
               vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
               vim.g.gruvbox_material_current_word = 'grey background'
               vim.g.gruvbox_material_better_performance = 1

               vim.cmd 'colorscheme gruvbox-material'
               vim.cmd('source ~/.config/nvim/lua/plugins_config/gruvbox-material.vim')
            end
         }
      end, --}}}
      -- gruvbox-material-light {{{
      ['gruvbox-material-light'] = function()
         use {
            'sainnhe/gruvbox-material',
            after = 'treesitter',
            config = function()
               vim.o.background = 'light'

               -- Set the color palette used in this color scheme.
               -- material : material palette with soft contrast;
               -- mix      : the mean of the other two;
               -- original : the original gruvbox palette.
               vim.g.gruvbox_material_palette = 'mix'

               -- Set contrast.
               -- available values: 'hard', 'medium'(default), 'soft'
               vim.g.gruvbox_material_background = 'soft'
               vim.g.gruvbox_material_enable_bold = 1
               vim.g.gruvbox_material_enable_italic = 1

               -- Available values: 'auto', 'red', 'orange', 'yellow',
               -- 'green', 'aqua', 'blue', 'purple'
               vim.g.gruvbox_material_cursor = 'blue'
               -- vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
               vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
               vim.g.gruvbox_material_current_word = 'grey background'
               vim.g.gruvbox_material_better_performance = 1

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
               vim.o.background = 'light'
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

   local theme = 'gruvbox-material-dark'
   -- local theme = 'gruvbox-material-light'
   -- local theme = 'tokyonight'
   -- local theme = 'melange'
   -- local theme = 'mellow'
   -- local theme = 'moonshine'
   -- local theme = 'srcery'

   if color_themes[theme] then
      color_themes[theme]()
   end

   use { 'folke/lsp-colors.nvim',
      config = function()
         require('lsp-colors').setup({
           Error       = "#db4b4b",
           Warning     = "#e0af68",
           Information = "#0db9d7",
           Hint        = "#10B981"
         })
      end
   }
   ----------------------------------------------------}}}

   -- --------------------- Statusline ----------------------
   -- use { 'glepnir/galaxyline.nvim',
   --    branch = 'main',
   --    config = function() require'statusline' end,
   --    requires = {'kyazdani42/nvim-web-devicons'}
   -- }
   -- -------------------------------------------------------

   --------------- Vim additional modules ----------------

   -- bufferline / tabline

   use { 'romgrk/barbar.nvim',
      as = 'barbar-tabline',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function()
         -- vim.cmd('source ~/.config/nvim/lua/plugins_config/barbar.vim')
         require('plugins_config/barbar')
      end
   }

   -- use { 'akinsho/nvim-bufferline.lua',
   --    requires = 'kyazdani42/nvim-web-devicons',
   --    config = function() require('plugins_config/nvim-bufferline') end
   -- }

   -- -- visualize undo tree
   -- use { 'mbbill/undotree',
   --    config = function()
   --       vim.g.undotree_HighlightChangedWithSign = 0
   --       vim.g.undotree_WindowLayout = 2
   --    end
   -- }

   -- -- another undo tree visualizer
   -- use 'simnalamburt/vim-mundo'

   use { 'folke/which-key.nvim',
      as = 'which-key',
      config = function()
         require('which-key').setup {
            spelling = {
               enabled = true,  -- Enabling this will show WhichKey when pressing z=
                                --   to select spelling suggestions.
               suggestions = 20, -- How many suggestions should be shown in the list?
            },
         }
      end
   }

   use { 'gelguy/wilder.nvim',
      requires = {
         'roxma/nvim-yarp',
         'roxma/vim-hug-neovim-rpc'
      },
      run = ':UpdateRemotePlugins',
      -- event = 'CmdlineEnter',
      config = function()
         vim.cmd('source ~/.config/nvim/lua/plugins_config/wilder.vim')
      end
   }

   use { 'kevinhwang91/nvim-bqf', as = 'better-quickfix' }

   -- <leader>? : 40-column cheat sheet
   use { 'anuvyklack/vim-cheat40', as = 'cheat40' } -- my fork

   -- -- TODO Need to rewrite in lua.
   -- use { 'camspiers/lens.vim',
   --    requires = 'camspiers/animate.vim',
   --    config = function()
   --       vim.cmd('source ~/.config/nvim/lua/plugins_config/lens.vim')
   --    end
   -- }

   -- -- foldtext customization
   -- use 'scr1pt0r/crease.vim'

   use { 'norcalli/nvim-colorizer.lua',
      as = 'colorizer',
      ft = {'vim', 'lua', 'conf', 'tmux', 'kitty', 'vifm', 'markdown'},
      config = function()
         require'colorizer'.setup {
            'markdown'
         }
      end
   }

   -- Execute :StartupTime to get an averaged startup profile.
   use { 'tweekmonster/startuptime.vim',
      cmd = 'StartupTime'
   }

   -------------------------------------------------------

   --                   File manager                   {{{
   -------------------------------------------------------

    -- use { 'kyazdani42/nvim-tree.lua',
    --    requires = 'kyazdani42/nvim-web-devicons',
    --    config = function() require('plugins_config/nvim-tree') end
    -- }

    -- use 'tpope/vim-vinegar'

    -- use { 'ms-jpq/chadtree',
    --    branch = 'chad',
    --    run = 'python3 -m chadtree deps && nvim --headless +"CHADdeps | exit"',
    --    cmd = {'CHADopen', 'CHADdeps', 'CHADhelp'}
    -- }

    -- -- Total Commander inspired file manager
    -- use { 'ripxorip/bolt.nvim',
    --    run = ':UpdateRemotePlugins'
    -- }

    -- use { 'luukvbaal/nnn.nvim',
    --    config = function() require('plugins_config/nnn-nvim') end
    -- }

   use { 'mcchrish/nnn.vim',
      config = function() require('plugins_config/nnn-vim') end
   }

   -- use { 'is0n/fm-nvim',
   --    config = function() require('plugins_config/fm-nvim') end
   -- }

   use { 'vifm/vifm.vim',
      -- TODO Open issue to packer: help tags not generated automaticaly.
      run = ':helptags ALL',
   }

   ----------------------------------------------------}}}

   --              Lua plugins development             {{{
   -------------------------------------------------------

   use { 'rafcamlet/nvim-luapad',
      as = 'luapad',
      config = function()
         vim.cmd [[
            command! LuaAttach lua require('luapad').attach()
            command! LuaDetach lua require('luapad').detach()
            command! LuaToggle lua require('luapad').toggle()
         ]]
      end
   }

   -- use { 'tjdevries/nlua.nvim' }

   -- -- Better Lua syntax highlighting
   -- use { 'euclidianAce/BetterLua.vim', as = 'syntax-BetterLua' }

   ----------------------------------------------------}}}

  --                      Orgmode                     {{{
  -------------------------------------------------------

  -- use { 'kristijanhusak/orgmode.nvim',
  --    config = function()
  --       require('orgmode').setup{
  --          org_hide_emphasis_markers = true,
  --       }
  --    end
  -- }

  use { 'vhyrro/neorg',
     requires = "nvim-lua/plenary.nvim",
     after = "treesitter",
     branch = 'unstable',
     setup = function() -- {{{
        -- WARNING Temporary code block until neorg treesitter module
        --         will be merged into `nvim-treesitter` repository.
        local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()

        parser_configs.norg = {
           install_info = {
              url = 'https://github.com/vhyrro/tree-sitter-norg',
              -- files = { "src/parser.c" },
              files  = { 'src/parser.c', 'src/scanner.cc' },
              branch = 'main'
           },
        }
     end, -- }}}
     config = function() require('plugins_config/neorg') end
  }

  ----------------------------------------------------}}}

   --                 Tmux integration                {{{
   ------------------------------------------------------

   -- use {
   --    'anuvyklack/vim-tmux-navigator',  -- my fork
   --    -- 'christoomey/vim-tmux-navigator',  -- original
   --
   --    config = function()
   --       -- Activate autoupdate on exit.
   --       vim.g.tmux_navigator_save_on_switch = 0
   --
   --       -- Disable vim->tmux navigation when the Vim pane is zoomed in tmux.
   --       vim.g.tmux_navigator_disable_when_zoomed = 1
   --    end
   -- }

   -- use 'tmux-plugins/vim-tmux-focus-events'
   -- use 'tmux-plugins/vim-tmux'
   ---------------------------------------------------}}}

   -- --                      Pandoc                      {{{
   -- -------------------------------------------------------
   -- use { 'vim-pandoc/vim-pandoc',
   --    ft = {'markdown', 'pandoc'},
   --    requires = {'vim-pandoc/vim-pandoc-syntax', opt = true},
   --    config = function()
   --       vim.cmd('source ~/.config/nvim/lua/plugins_config/pandoc.vim')
   --    end
   -- }
   -- ----------------------------------------------------}}}

   --          Русский язык (Switch language)          {{{
   -------------------------------------------------------
   use { 'lyokha/vim-xkbswitch',
      as = 'xkbswitch',
      config = function()
         vim.g.XkbSwitchEnabled = 1
         vim.g.XkbSwitchLib = '/usr/local/lib/libg3kbswitch.so'
         vim.g.XkbSwitchAssistNKeymap = 1  -- for commands r and f
         vim.g.XkbSwitchAssistSKeymap = 1  -- for search lines
         -- vim.g.XkbSwitchIMappings = {'ru'}
      end
   }
   -- use 'powerman/vim-plugin-ruscmd'
   ----------------------------------------------------}}}

end)

-- vim: fml=1 fdm=marker
