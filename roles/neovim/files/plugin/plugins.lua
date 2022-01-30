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

--                       Install and setup Packer                            {{{
--------------------------------------------------------------------------------
local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
   vim.fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', '--depth', '1', install_path})
   vim.api.nvim_command('packadd packer.nvim')
end

require('packer').init { --{{{
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
} --}}}

-- -- Auto compile when there are changes in plugins.lua
-- -- vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'
-- vim.cmd [[
--   augroup Packer
--     autocmd!
--     autocmd BufWritePost plugins.lua PackerCompile
--   augroup end
-- ]]

-----------------------------------------------------------------------------}}}

require('packer').startup(function()
   use 'wbthomason/packer.nvim' -- Packer can manage itself

   use 'lewis6991/impatient.nvim' -- Improve startup time for Neovim.

   --                          Key Mappings                               {{{
   -------------------------------------------------------------------------

   -- use { 'folke/which-key.nvim', as = 'which-key', -- WhichKey
   --    config = function ()
   --       require('which-key').setup {
   --          plugins = {
   --             spelling = { -- Enabling this module will show WhichKey when
   --                          -- pressing z= to select spelling suggestions.
   --                enabled = false,
   --                suggestions = 20, -- How many suggestions should be shown in the list?
   --             },
   --             presets = {
   --                operators = true,
   --             },
   --          },
   --          operators = {
   --             gc = "Comments"
   --          },
   --          -- ignore_missing = true, -- Enable this to hide mappings for which you
   --          --                        -- didn't specify a label.
   --          hidden = { -- hide mapping boilerplate
   --             "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ",
   --             "<SNR>", "<Plug>"
   --          },
   --       }
   --    end
   -- }

   --------------------------------------------------------------------}}}

   --                           Treesitter                               {{{
   -------------------------------------------------------------------------
   use { 'nvim-treesitter/nvim-treesitter', as = 'treesitter',
      requires = {
         {'nvim-treesitter/nvim-treesitter-textobjects', as = 'treesitter-textobjects'},
         {'RRethy/nvim-treesitter-textsubjects',         as = 'treesitter-textsubjects'},
         {'nvim-treesitter/nvim-treesitter-refactor',    as = 'treesitter-refactor'},
         {'romgrk/nvim-treesitter-context',              as = 'treesitter-context'},
         {'JoosepAlviste/nvim-ts-context-commentstring', as = 'treesitter-context-commentstring'},
         {'nvim-treesitter/playground',                  as = 'treesitter-playground'},
         {'p00f/nvim-ts-rainbow',                        as = 'treesitter-rainbow'},
      },
      run = ':TSUpdate',
      config = function() require('plugins_config/treesitter') end
   }

   -- use 'wellle/context.vim'   -- Vscode breadcrumbs analog
   ----------------------------------------------------------------------}}}

   --                              LSP                                   {{{
   -------------------------------------------------------------------------

   use { 'neovim/nvim-lspconfig', as = 'lspconfig',
      config = function() require 'plugins_config/lspconfig' end,
      requires = {
         {  -- LSP servers installer.
            'williamboman/nvim-lsp-installer', as = 'lsp-installer'
         },
         {  -- Setup for lua and plugins development.
            'folke/lua-dev.nvim', as = 'lua-dev'
         }
      }
   }

   -- Lsp signature hint when you type.
   use { 'ray-x/lsp_signature.nvim', as = 'lsp-signature',
      config = function() require('plugins_config/lsp_signature') end
   }

   -- use { 'glepnir/lspsaga.nvim',  -- original
   use { 'tami5/lspsaga.nvim',  -- maintained fork
      as = 'lspsaga',
      config = function() require('plugins_config/lspsaga') end
   }

   use { 'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
      cmd = {'Trouble', 'TroubleToggle'},
      config = function() require('plugins_config/trouble') end
   }

   -- use 'nvim-lua/diagnostic-nvim'

   -- use { 'jubnzv/virtual-types.nvim', as = 'virtual-types' }

   -- use { 'simrat39/symbols-outline.nvim', as = 'symbols-outline',
   --    config = function() require('plugins_config/symbols-outline') end
   -- }

   -- use { "liuchengxu/vista.vim" }

   use { 'ahmedkhalf/project.nvim',
      config = function()
         require('project_nvim').setup {
            detection_methods = { 'lsp', 'pattern', },
            patterns = {
               '.git', '_darcs', '.hg', '.bzr', '.svn', 'Makefile', 'package.json',
               '>.config', '>roles'
            },
            silent_chdir = true, -- When set to false, you will get a message
                                 -- when project.nvim changes your directory.
         }
      end
   }

   -- use 'nvim-lua/lsp_extensions.nvim'
   -- use 'tjdevries/nlua.nvim'

   ----------------------------------------------------------------------}}}

   --                 Completion, Autopairs, Snippets                    {{{
   -------------------------------------------------------------------------

   -- Autopairs {{{
   use { 'windwp/nvim-autopairs', as = 'autopairs',
      config = function()
         require('nvim-autopairs').setup()
      end
   }
   -- }}}

   -- cmp {{{

   use { 'L3MON4D3/LuaSnip',
      config = function() require('plugins_config/luasnip') end,
   }

   -- The community driven  collection of snippets.
   -- use { 'rafamadriz/friendly-snippets' }

   use { 'hrsh7th/nvim-cmp', as = 'cmp',
      config = function() require('plugins_config/cmp') end,
      requires = {
         {  -- Icons in completion menu.
            'onsails/lspkind-nvim', as = 'lspkind',
            config = function() require('plugins_config/lspkind-nvim') end
         },
         'saadparwaiz1/cmp_luasnip', -- source for LuaSnip snippet plugin
         'hrsh7th/cmp-nvim-lsp', -- source for neovim's Lua runtime API such 'vim.lsp.*'
         'hrsh7th/cmp-buffer',
         'hrsh7th/cmp-path',
         'hrsh7th/cmp-cmdline',
      }
   }

   --}}}

   -- -- coq {{{
   -- use { 'ms-jpq/coq_nvim', branch = 'coq',
   --    after = 'autopairs',
   --    requires = {
   --       { -- 9000+ Snippets
   --          'ms-jpq/coq.artifacts', branch = 'artifacts'
   --       },
   --       { -- https://github.com/ms-jpq/coq.thirdparty
   --          'ms-jpq/coq.thirdparty', branch = '3p'
   --       }
   --    },
   --    setup  = function() require('plugins_config/coq_nvim').setup() end,
   --    config = function() require('plugins_config/coq_nvim').config() end,
   -- }
   -- --}}}
   --
   -- -- wilder {{{
   -- use { 'gelguy/wilder.nvim',
   --    requires = {
   --       'roxma/nvim-yarp',
   --       'roxma/vim-hug-neovim-rpc'
   --    },
   --    run = ':UpdateRemotePlugins',
   --    event = 'CmdlineEnter',
   --    config = function() vim.cmd("source ~/.config/nvim/lua/plugins_config/wilder.vim") end
   -- }
   -- --}}}

   ----------------------------------------------------------------------}}}

   --                  DAP (Debug Adapter Protocol)                      {{{
   -------------------------------------------------------------------------
   -- use { "mfussenegger/nvim-dap", as = 'dap' }
   -- use { "rcarriga/nvim-dap-ui",  as = 'dap-ui' }
   ----------------------------------------------------------------------}}}

   ---------------------------- Text editing -------------------------------

   use 'matze/vim-move'       -- перемещение строк и частей строк
   use 'wellle/targets.vim'   -- plugin that provides additional text objects
   use 'tpope/vim-unimpaired' -- Different bidirectional motions: switch
                              -- buffers, add blank lines, etc.
   use 'junegunn/vim-easy-align'

   -- use 'othree/eregex.vim'     -- Perl style regexp notation for Vim

   -- Comments {{{

   -- Also define `gc` - comment textobject.
   -- Doesn't work in visual mode. Works in operator mode (:help mapmode-o).
   use { 'echasnovski/mini.nvim',
      config = function()
         require('mini.comment').setup{}
         --
         which_key{
            gc = { name = 'Comment' },
            gcc = 'Comment out current line',
            ['gc*'] = "which_key_ignore",
            ['gc#'] = "which_key_ignore",
         }
      end
   }

   -- use 'tomtom/tcomment_vim'

   -- }}}

   -- Surround {{{

   use { 'tpope/vim-surround', as = 'surround',
      requires = 'tpope/vim-repeat'
   }

   -- -- Not yet as usable as tpope/vim-surround.
   -- use { 'blackCauldron7/surround.nvim', as = 'surround',
   --    config = function()
   --       require"surround".setup {
   --          mappings_style = "surround",
   --          load_autogroups = true,
   --          map_insert_mode = false,
   --       }
   --    end
   -- }

   -- }}}

   -- Makes * operator works the way it should by default.
   use { 'haya14busa/vim-asterisk', as = 'asterisk',
      config = function()
         vim.g['asterisk#keeppos'] = 0 -- Keep cursor position inside word between jumps.
         require('keybindings').asterisks()
      end
   }

   -- use { 'kevinhwang91/nvim-hlslens', as = 'hlslens',
   --    config = function() require('plugins_config/hlslens') end
   -- }

   -- Подсвечивать и удалять висящие пробелы в конце строк
   use { 'ntpeters/vim-better-whitespace', as = 'better-whitespace',
      config = function()
         -- vim.cmd("exe 'source'..stdpath('config')..'/lua/plugins_config/better-whitespace.vim'")
         vim.cmd("source ~/.config/nvim/lua/plugins_config/better-whitespace.vim")
      end
   }

   use { 'AndrewRadev/splitjoin.vim', as = 'splitjoin' }

   -- -- The same as 'splitjoin', but in lua and may be some day will using treesitter.
   -- use { 'AckslD/nvim-revJ.lua',
   --    requires = 'wellle/targets.vim',
   --    config = function()
   --       require("revj").setup{
   --          keymaps = {
   --              operator = '<Leader>J', -- for operator (+motion)
   --              line = '<Leader>j', -- for formatting current line
   --              visual = '<Leader>j', -- for formatting visual selection
   --          },
   --       }
   --    end
   -- }

   -- Multiple cursors
   -- https://github.com/mg979/vim-visual-multi
   use { 'mg979/vim-visual-multi', as = 'multiple-cursors'}

   ----------------------------- Clipboard ---------------------------------

   -- use { 'AckslD/nvim-neoclip.lua',
   --    requires = {'tami5/sqlite.lua', module = 'sqlite'},
   --    config = function()
   --       require('neoclip').setup()
   --    end,
   -- }

   --------------------------- Visual tweaks -------------------------------

   -- Подсвечивает все такие же слова как и слово под курсором.
   use { 'RRethy/vim-illuminate' }

   -- use 'itchyny/vim-cursorword'

   -- -- Найденный текст пульсирует
   -- use { 'inside/vim-search-pulse' }

   -- Indentation guides {{{
   use { 'lukas-reineke/indent-blankline.nvim',
      ft = {
         'python',
         -- 'lua'
      },
      config = function()
         require("indent_blankline").setup {
            filetype = {
               'python',
               -- 'lua'
            },
            use_treesitter = true,
            show_current_context = true,
            show_current_context_start = false,
            show_end_of_line = true,
            context_highlight_list = {'Error', 'Warning'},
            show_first_indent_level = false,
            max_indent_increase = 1,
            show_trailing_blankline_indent = false,
         }
      end
   }
   --}}}

   -------------------- Windows and buffers managment ----------------------

   -- use 'matbme/JABS.nvim'

   use { 'jlanzarotta/bufexplorer',
      -- requires = 'ryanoasis/vim-devicons', -- Install to enable devicons.
      config = function()
         vim.g.bufExplorerDisableDefaultKeyMapping = 1 -- Disable default mappings.
         vim.g.bufExplorerFindActive = 0  -- Do not go to active window.
         vim.g.bufExplorerShowNoName = 1  -- Show "No Name" buffers.
         vim.g.bufExplorerShowRelativePath = 1 -- Show relative paths.
      end
   }

   use { 'roxma/vim-window-resize-easy', as = 'window-resize-easy' }

   -- use { 'simeji/winresizer',
   --   config = function()
   --     vim.g.winresizer_vert_resize  = 1
   --     vim.g.winresizer_horiz_resize = 1
   --     vim.g.winresizer_start_key = '<leader>w'
   --     -- require("util").set_keymap('n', '<leader>w', 'Window resize mode', '<cmd>WinResizerStartResize<CR>', {noremap = true, silent = true})
   --   end
   -- }

   -- use 'zhaocai/GoldenView.Vim'
   -- use { 'RobertAudi/GoldenView.vim', opt = true }

   ----------------------------- Movements ---------------------------------

   -- use { 'easymotion/vim-easymotion', as = 'easymotion',
   --    config = function()
   --       vim.cmd("source ~/.config/nvim/lua/plugins_config/easymotion.vim")
   --    end
   -- }

   use { 'phaazon/hop.nvim', as = 'easymotion-hop',
      config = function()
         require'hop'.setup {
            winblend = 30,
            -- keys = 'asdghklqwertyuiopzxcvbnmfj'
            -- keys = 'asdghklqwertyuiopzxcvfjbnm'
            keys = 'asdghklqwertyuiopzxcvfjbn',
            uppercase_labels = false, -- Display labels as uppercase.
         }
         require('keybindings').hop()
      end
   }

   -- use {'ggandor/lightspeed.nvim', }

   --                          Smooth scroll                             {{{
   -------------------------------------------------------------------------

   -- https://github.com/psliwka/vim-smoothie
   use { 'psliwka/vim-smoothie',
      config = function()
         -- Time (in milliseconds) between subseqent screen/cursor postion
         -- updates.  Lower value produces smoother animation.
         vim.g.smoothie_update_interval = 20
         --
         -- Base scrolling speed (in lines per second), to be taken into account
         -- by the velocity calculation algorithm.  Can be decreased to achieve
         -- slower (and easier to follow) animation.
         vim.g.smoothie_base_speed = 7
      end
   }

   -- use { 'karb94/neoscroll.nvim',
   --    config = function() require('plugins_config/neoscroll') end
   -- }

   ----------------------------------------------------------------------}}}

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

   -------------------------------------------------------------------------

   ---------------------------- IDE features -------------------------------

   -- use 'tpope/vim-apathy'  -- Make 'gf' keybinding work in different filetypes.
   --                         -- For instructions of what it can do more look:
   --                         -- https://github.com/tpope/vim-apathy
   --                         -- and
   --                         -- :help include-search

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
      requires = 'nvim-lua/plenary.nvim',
      config = function()
         require('neogit').setup{}
      end
   }

   --------------------------- Fuzzy finder --------------------------------
   use { 'nvim-telescope/telescope.nvim', as = 'telescope',
      commit = '860cc65',
      requires = {
         'nvim-lua/plenary.nvim',
         { 'nvim-telescope/telescope-fzf-native.nvim', as = 'telescope-fzf-native',
            run = 'make'
         },
         -- 'famiu/bufdelete.nvim',
      },
      config = function() require('plugins_config/telescope') end,
      -- event = 'VimEnter',
      -- cmd = 'Telescope',
      -- keys = {'<leader>f', '<leader>fh'}
   }

   ----------------------------- Statusline --------------------------------
   -- use { 'glepnir/galaxyline.nvim',
   --    branch = 'main',
   --    config = function() require'statusline' end,
   --    requires = {'kyazdani42/nvim-web-devicons'}
   -- }
   -------------------------------------------------------------------------

   ----------------------- Vim additional modules --------------------------

   -- bufferline / tabline

   use { 'romgrk/barbar.nvim', as = 'barbar-tabline',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function()
         require('plugins_config/barbar')
      end
   }

   use { 'anuvyklack/pretty-fold.nvim', as = 'pretty-fold', branch = 'nightly',
   -- use { '~/Git/my_neovim_plugins/pretty-fold.nvim', as = 'pretty-fold',
      config = function()
         vim.opt.fillchars:append('fold:•')
         require('pretty-fold').setup{
            -- marker = { comment_signs = 'spaces' },
            marker = { process_comment_signs = 'spaces' },
            expr = { process_comment_signs = false }
         }
         require('pretty-fold.preview').setup{
            -- border = 'shadow'
         }
      end
   }

   use { 'sindrets/diffview.nvim',
      requires = 'nvim-lua/plenary.nvim'
   }

   -- Marks
   -- use 'kshenoy/vim-signature'  -- display and navigate marks
   use { 'chentau/marks.nvim',
      config = function()
         require'marks'.setup {
            -- force_write_shada = true,
            default_mappings = true,
            -- builtin_marks = { ".", "<", ">", "^", "'" },
            signs = true,
            excluded_filetypes = { 'gitcommit' },
            mappings = {}
         }
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

   use { 'kevinhwang91/nvim-bqf', as = 'better-quickfix',
      ft = 'qf'
   }

   use { 'https://gitlab.com/yorickpeterse/nvim-pqf', as = 'pretty-quickfix',
      ft = 'qf',
      config = function() require('pqf').setup() end
   }

   -- <leader>? : 40-column cheat sheet
   use { 'anuvyklack/vim-cheat40', as = 'cheat40' } -- my fork

   -- -- TODO Need to rewrite in lua.
   -- use { 'camspiers/lens.vim',
   --    requires = 'camspiers/animate.vim',
   --    config = function() vim.cmd("source ~/.config/nvim/lua/plugins_config/lens.vim") end
   -- }

   -- -- foldtext customization
   -- use 'scr1pt0r/crease.vim'

   use { 'norcalli/nvim-colorizer.lua', as = 'colorizer',
      ft = {'vim', 'lua', 'conf', 'tmux', 'kitty', 'vifm', 'markdown'},
      config = function()
         require'colorizer'.setup {
            'markdown'
         }
      end
   }

   use { 'anuvyklack/help-vsplit.nvim', as = 'help-vsplit',
   -- use { '~/Git/my_neovim_plugins/help-vsplit.nvim', as = 'help-vsplit',
      config = function()
         require('help-vsplit').setup{
            side = 'right' -- or 'left'
         }
      end
   }

   -- Execute :StartupTime to get an averaged startup profile.
   use { 'tweekmonster/startuptime.vim', as = 'startuptime',
      cmd = 'StartupTime'
   }

   -- -- use 'simnalamburt/vim-mundo'  -- another undo tree visualizer
   -- use { 'mbbill/undotree',         -- visualize undo tree
   --    config = function()
   --       vim.g.undotree_HighlightChangedWithSign = 0
   --       vim.g.undotree_WindowLayout = 2
   --    end
   -- }

   -------------------------------------------------------------------------

   ----------------- Syntaxes and programming languages --------------------

   -- Show syntax highlighting attributes of character under cursor.
   use 'vim-scripts/SyntaxAttr.vim'

   -- Automatically adjusts 'shiftwidth' and 'expandtab' heuristically
   -- based on the current file.
   use { 'tpope/vim-sleuth',
      cmd = { 'Sleuth', 'verbose Sleuth' },
      config = function()
         vim.g.sleuth_automatic = 0 -- Turn off automatic detection, потому что
                                    -- думает за меня и думает лишнего.
      end
   }

   -- Подсветка синтаксисов для разных языков.
   use { 'sheerun/vim-polyglot',
      setup = function()  -- run before plugin load
         -- This variable should be declared before polyglot is loaded!
         vim.g.polyglot_disabled = {
            'sensible',
            'autoindent', -- use https://github.com/tpope/vim-sleuth instead
            -- 'help',  -- because it force set 'expandtab'
            -- 'markdown',
            -- 'gitignore',
            -- 'txt',
         }
      end
   }

   use { 'Neui/cmakecache-syntax.vim', as = 'syntax-cmakecache' }
   -- use { 'zinit-zsh/zinit-vim-syntax', ft = 'zsh' } -- zinit syntaxis

   use { 'anuvyklack/vim-dealii-prm', as = 'syntax-dealii-prm',
      -- event = 'BufNewFile,BufRead *.prm'
   }

   -- Kitty terminal conf file syntax highlight.
   use { "fladson/vim-kitty", as = 'syntax-kitty-conf' }

   -------------------------------------------------------------------------

   --                           File manager                             {{{
   -------------------------------------------------------------------------

   use { 'kyazdani42/nvim-tree.lua',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function() require('plugins_config/nvim-tree') end
   }

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

   ----------------------------------------------------------------------}}}

   --                      Lua plugins development                       {{{
   -------------------------------------------------------------------------
   use { 'rafcamlet/nvim-luapad', as = 'luapad',
      config = function()
         local command = vim.api.nvim_add_user_command
         local luapad = require('luapad')
         command('LuaAttach', luapad.attach, { bang = true })
         command('LuaDetach', luapad.detach, { bang = true })
         command('LuaToggle', luapad.toggle, { bang = true })
      end
   }
   ----------------------------------------------------------------------}}}

   --                              Orgmode                               {{{
   -------------------------------------------------------------------------

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
      config = function() require('plugins_config/neorg') end
   }

   ----------------------------------------------------------------------}}}

   --                         Tmux integration                           {{{
   -------------------------------------------------------------------------

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

   -- use 'tmux-plugins/vim-tmux-focus-events'
   -- use 'tmux-plugins/vim-tmux'
   ----------------------------------------------------------------------}}}

   --                              Pandoc                                {{{
   -------------------------------------------------------------------------
   -- use { 'vim-pandoc/vim-pandoc',
   --    requires = {'vim-pandoc/vim-pandoc-syntax', opt = true},
   --    ft = {'markdown', 'pandoc'},
   --    config = function()
   --       vim.cmd("source ~/.config/nvim/lua/plugins_config/pandoc.vim")
   --    end
   -- }
   ----------------------------------------------------------------------}}}

   --                   Русский язык (Switch language)                   {{{
   -------------------------------------------------------------------------
   use { 'lyokha/vim-xkbswitch', as = 'xkbswitch',
      config = function()
         vim.g.XkbSwitchEnabled = 1
         vim.g.XkbSwitchLib = '/usr/local/lib/libg3kbswitch.so'
         vim.g.XkbSwitchAssistNKeymap = 1  -- for commands r and f
         vim.g.XkbSwitchAssistSKeymap = 1  -- for search lines
         -- vim.g.XkbSwitchIMappings = {'ru'}

         which_key{
            gh = 'which_key_ignore',
            gH = 'test description',
            ['g<C-H>'] = 'which_key_ignore'
         }
      end
   }
   -- use 'powerman/vim-plugin-ruscmd'
   ----------------------------------------------------------------------}}}

   --                           Color scheme                             {{{
   -------------------------------------------------------------------------
   --                 ███                                    ██
   --                ░░██                                   ░██
   --   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
   --  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
   -- ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
   -- ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
   -- ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
   --  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░


   local color_themes = {
      -- gruvbox-material-dark {{{
      ['gruvbox-material-dark'] = function()
         use { 'sainnhe/gruvbox-material',
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
               vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
               -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
               vim.g.gruvbox_material_current_word = 'grey background'
               vim.g.gruvbox_material_better_performance = 1

               vim.cmd 'colorscheme gruvbox-material'
               vim.cmd("source ~/.config/nvim/lua/plugins_config/gruvbox-material.vim")
            end
         }
      end, --}}}
      -- gruvbox-material-light {{{
      ['gruvbox-material-light'] = function()
         use { 'sainnhe/gruvbox-material',
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
               vim.cmd("source ~/.config/nvim/lua/plugins_config/gruvbox-material.vim")
            end
         }
      end, --}}}
      -- melange {{{
      ['melange'] = function()
         use { 'savq/melange',
            config = function()
               vim.o.background = 'light'
               vim.cmd 'colorscheme melange'
            end
         }
      end, --}}}
      -- moonshine {{{
      ['moonshine'] = function()
         use { 'karoliskoncevicius/moonshine-vim',
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
         use { 'adigitoleo/vim-mellow', as = 'mellow',
            config = function()
               vim.o.background = 'light'
               -- vim.o.background = 'dark'
               vim.cmd 'colorscheme mellow'
               vim.cmd("source ~/.config/nvim/lua/plugins_config/mellow.vim")
            end
         }
      end, --}}}
      -- tokyonight {{{
      ['tokyonight'] = function()
         use { 'folke/tokyonight.nvim',
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
         require('lsp-colors').setup{
            Error       = "#db4b4b",
            Warning     = "#e0af68",
            Information = "#0db9d7",
            Hint        = "#10B981"
         }
      end
   }
   ----------------------------------------------------------------------}}}

end)

-- vim: fml=1 fdm=marker
