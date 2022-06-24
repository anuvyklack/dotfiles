--         ███                 ██                    ███
--        ░░██                ░░                    ░░██
--  ██████ ░██ ██   ██  ██████ ██ ██████   ██████    ░██ ██   ██  █████
-- ░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░     ░██░██  ░██ ░░░░░██
-- ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████     ░██░██  ░██  ██████
-- ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██    ░██░██  ░██ ██░░░██
-- ░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████  ██ ░██░░█████ ░░███████
-- ░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░  ░░  ░░  ░░░░░   ░░░░░░░
-- ░░                  ░░░░░

--                       Install and setup Packer                            {{{
--------------------------------------------------------------------------------
local packer = require('packer')
local use = packer.use
local join_paths = require('packer/util').join_paths

packer.init { --{{{
   compile_path = join_paths(vim.fn.stdpath('config'), 'plugin', '1_packer_compiled.lua'),
   display = {
      -- open_cmd = '90vnew [packer]',  -- set the width of the packer split
      open_fn = function()
         return require("packer.util").float({
            border = "rounded",
            -- width  = math.ceil(vim.o.columns * 0.7),
            height = math.ceil(vim.o.lines * 0.8)
         })
      end,
      working_sym = '', --   plugin being installed/updated
      error_sym = '',   -- plugin with an error in installation/updating
      done_sym = '',    --  plugin which has completed installation/updating
   },
   git = {
      clone_timeout = 600  -- timeout, in seconds, for git clones
   }
} --}}}

-- -- Auto compile when there are changes in plugins.lua
-- vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'

-----------------------------------------------------------------------------}}}

use { 'wbthomason/packer.nvim', opt = true }

use 'lewis6991/impatient.nvim' -- Improve startup time for Neovim.

--                          Color scheme                              {{{
-------------------------------------------------------------------------
--                 ███                                    ██
--                ░░██                                   ░██
--   █████   █████ ░██  █████  ██████      ██████  █████ ░██████   █████  ██████████   █████
--  ██░░░██ ██░░░██░██ ██░░░██░░██░░█     ██░░░░  ██░░░██░██░░░██ ██░░░██░░██░░██░░██ ██░░░██
-- ░██  ░░ ░██  ░██░██░██  ░██ ░██ ░     ░░█████ ░██  ░░ ░██  ░██░███████ ░██ ░██ ░██░███████
-- ░██   ██░██  ░██░██░██  ░██ ░██        ░░░░░██░██   ██░██  ░██░██░░░░  ░██ ░██ ░██░██░░░░
-- ░░█████ ░░█████ ░██░░█████  ███        ██████ ░░█████ ░██  ░██░░█████  ███ ░██ ░██░░█████
--  ░░░░░   ░░░░░  ░░  ░░░░░  ░░░        ░░░░░░   ░░░░░  ░░   ░░  ░░░░░  ░░░  ░░  ░░  ░░░░░

use 'rktjmp/lush.nvim'

local color_themes = {
   -- gruvbox-material {{{
   ['gruvbox-material'] = {
      dark = function() -- {{{
         use { 'sainnhe/gruvbox-material',
            config = function()
               vim.o.background = 'dark'

               -- Set contrast.
               -- available values: 'hard', 'medium'(default), 'soft'
               vim.g.gruvbox_material_background = 'medium'

               -- Set the color palette used in this color scheme.
               -- material : material palette with soft contrast;
               -- mix      : the mean of the other two;
               -- original : the original gruvbox palette.
               vim.g.gruvbox_material_palette = 'mix'

               vim.g.gruvbox_material_enable_bold = 1
               vim.g.gruvbox_material_enable_italic = 1

               -- Available values: 'auto', 'red', 'orange', 'yellow',
               -- 'green', 'aqua', 'blue', 'purple'
               vim.g.gruvbox_material_cursor = 'aqua'

               -- 'colored' or 'grey'
               vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
               vim.g.gruvbox_material_diagnostic_text_highlight = 1 -- 0 or 1

               -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
               vim.g.gruvbox_material_current_word = 'grey background'
               vim.g.gruvbox_material_better_performance = 1

               vim.cmd 'colorscheme gruvbox-material'
               vim.cmd("source ~/.config/nvim/lua/plugins-config/gruvbox-material.vim")
            end
         }
      end, -- }}}
      light = function() -- {{{
         use { 'sainnhe/gruvbox-material',
            config = function()
               vim.o.background = 'light'
               vim.g.gruvbox_material_background = 'soft'
               vim.g.gruvbox_material_palette = 'mix'
               vim.g.gruvbox_material_enable_bold = 1
               vim.g.gruvbox_material_enable_italic = 1
               vim.g.gruvbox_material_cursor = 'aqua'

               -- 'colored' or 'grey'
               vim.g.gruvbox_material_diagnostic_virtual_text = 'colored'
               vim.g.gruvbox_material_diagnostic_text_highlight = 1 -- 0 or 1

               -- vim.g.gruvbox_material_diagnostic_virtual_text = 'grey'
               vim.g.gruvbox_material_current_word = 'grey background'
               vim.g.gruvbox_material_better_performance = 1

               vim.cmd 'colorscheme gruvbox-material'
               vim.cmd("source ~/.config/nvim/lua/plugins-config/gruvbox-material.vim")
            end
         }
      end, -- }}}
   }, --}}}
   -- melange {{{
   ['melange'] = {
      dark = function() -- {{{
         use { 'savq/melange',
            config = function()
               vim.o.background = 'dark'
               vim.cmd 'colorscheme melange'
            end
         }
      end, -- }}}
      light = function() -- {{{
         use { 'savq/melange',
            config = function()
               vim.o.background = 'light'
               vim.cmd 'colorscheme melange'
            end
         }
      end -- }}}
   }, --}}}
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
      use { 'srcery-colors/srcery-vim', as = 'srcery',
         config = function()
            vim.cmd 'colorscheme srcery'
         end
      }
   end, --}}}
   -- mellow {{{
   mellow = {
      -- dark {{{
      dark = function()
         use { 'adigitoleo/vim-mellow', as = 'mellow',
            config = function()
               vim.o.background = 'dark'
               vim.cmd 'colorscheme mellow'
               vim.cmd 'source ~/.config/nvim/lua/plugins-config/mellow.vim'
            end
         }
      end,
      -- }}}
      -- light {{{
      light = function()
         -- My fork of vim-mellow scheme.
         -- https://github.com/adigitoleo/vim-mellow
         use { '~/code/neovim-plugins/colorsheme/mellow.nvim', as = 'mellow',
            config = function()
               vim.o.background = 'light'
               vim.cmd 'colorscheme mellow'
               vim.cmd("source ~/.config/nvim/lua/plugins-config/mellow.vim")
            end
         }
      end
      -- }}}
   }, --}}}
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
   -- everforest {{{
   everforest = function()
      use { 'sainnhe/everforest',
         config = function()
            vim.o.background = 'light'
            vim.g.everforest_background = 'soft'
            vim.g.everforest_better_performance = 1
            vim.cmd 'colorscheme everforest'
         end
      }
   end,
   -- }}}
   -- manuscript {{{
   manuscript = function()
      -- My fork of vim-paper scheme.
      -- https://gitlab.com/yorickpeterse/vim-paper.git
      use { '~/code/neovim-plugins/colorsheme/manuscript',
         config = function()
            vim.cmd 'colorscheme manuscript'
         end
      }
   end
   -- }}}
}

local colorscheme = { 'gruvbox-material', 'dark' }
-- local colorscheme = { 'gruvbox-material', 'light' }
-- local colorscheme = 'tokyonight'
-- local colorscheme = { 'melange', 'light' }
-- local colorscheme = { 'melange', 'dark' }
-- local colorscheme = { 'mellow', 'light' }
-- local colorscheme = { 'mellow', 'dark' }
-- local colorscheme = 'moonshine'
-- local colorscheme = 'srcery'
-- local colorscheme = 'everforest'
-- local colorscheme = 'manuscript'

local flavor; if type(colorscheme) == 'table' then
   colorscheme, flavor = colorscheme[1], colorscheme[2]
end
if color_themes[colorscheme] then
   if flavor then
      color_themes[colorscheme][flavor]()
   else
      color_themes[colorscheme]()
   end
end

use { 'folke/lsp-colors.nvim', --{{{
   config = function() require('lsp-colors').setup {
      Error       = "#db4b4b",
      Warning     = "#e0af68",
      Information = "#0db9d7",
      Hint        = "#10B981"
   } end
} --}}}
----------------------------------------------------------------------}}}

--                          Key Mappings                              {{{
-------------------------------------------------------------------------

-- use { 'folke/which-key.nvim', -- original
--    as = 'which-key', -- WhichKey
--    config = function() require('plugins-config/which-key') end
-- }

-- use { 'anuvyklack/nvim-keymap-amend', as = 'keymap-amend' }
use { '~/code/neovim-plugins/nvim-keymap-amend', as = 'keymap-amend' }

--------------------------------------------------------------------}}}

--                              LSP                                   {{{
-------------------------------------------------------------------------

use { 'neovim/nvim-lspconfig', as = 'lspconfig', --{{{
   config = function() require 'plugins-config/lsp_config' end,
   requires = {
      {  -- LSP servers installer.
         'williamboman/nvim-lsp-installer', as = 'lsp-installer'
      },
      {  -- Setup for lua and plugins development.
         'folke/lua-dev.nvim', as = 'lua-dev'
      }
   }
} --}}}

use { 'kosayoda/nvim-lightbulb', as = 'lightbulb', --{{{
   requires = { 'antoinemadec/FixCursorHold.nvim' },
   config = function() require 'plugins-config/lightbulb' end
} --}}}

use { 'weilbith/nvim-code-action-menu', as = 'code-action-menu',
   cmd = 'CodeActionMenu',
}

-- use { 'RishabhRD/nvim-lsputils', as = 'lsputils', --{{{
--    requires = 'RishabhRD/popfix',
--    config = function()
--       vim.lsp.handlers['textDocument/codeAction']     = require'lsputil.codeAction'.code_action_handler
--       vim.lsp.handlers['textDocument/references']     = require'lsputil.locations'.references_handler
--       -- vim.lsp.handlers['textDocument/definition']     = require'lsputil.locations'.definition_handler
--       vim.lsp.handlers['textDocument/declaration']    = require'lsputil.locations'.declaration_handler
--       vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
--       vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
--       vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
--       vim.lsp.handlers['workspace/symbol'] = require'lsputil.symbols'.workspace_handler
--    end
-- } --}}}

-- Lsp signature hint when you type.
use { 'ray-x/lsp_signature.nvim', as = 'lsp-signature',
   config = function() require('plugins-config/lsp_signature') end
}

use { 'j-hui/fidget.nvim', -- {{{
   config = function() require('fidget').setup {
      text = {
         -- spinner = 'line',
         spinner = 'dots',
         -- character shown when all tasks are complete
         done = "󰸞", -- f0e1e: 󰸞  (mdi-check-bold)
         commenced = "Started",    -- message shown when task starts
         completed = "Completed",  -- message shown when task completes
      },
      window = {
         blend = 0,  -- &winblend for the window
      },
      fmt = {
         stack_upwards = true,  -- list of tasks grows upwards
      }
   } end
} -- }}}

-- use { "https://git.sr.ht/~whynothugo/lsp_lines.nvim", --{{{
--    config = function()
--       require("lsp_lines").register_lsp_virtual_lines()
--       -- Disable virtual_text since it's redundant due to lsp_lines.
--       vim.diagnostic.config {
--          virtual_text = false,
--       }
--    end,
-- } --}}}

-- use { "narutoxy/dim.lua", --{{{
--   config = function() require('dim').setup({}) end
-- } --}}}

use { 'jubnzv/virtual-types.nvim' }

use { 'folke/trouble.nvim', --{{{
   requires = 'kyazdani42/nvim-web-devicons',
   -- cmd = { 'Trouble', 'TroubleToggle' },
   config = function() require 'plugins-config/trouble' end
} --}}}

use { 'liuchengxu/vista.vim',
   config = function() vim.cmd 'source ~/.config/nvim/lua/plugins-config/vista.vim' end
}

-- use { 'simrat39/symbols-outline.nvim', as = 'symbols-outline', --{{{
--    config = function()
--       require('plugins-config/symbols-outline')
--       vim.cmd 'source ~/.config/nvim/lua/plugins-config/symbols-outline.vim'
--    end
-- } --}}}

-- use { 'stevearc/aerial.nvim', as = 'aerial', --{{{
--    config = function() require("aerial").setup({
--       show_guides = false,
--    }) end
-- } --}}}

-- use { 'https://gitlab.com/yorickpeterse/nvim-dd.git', --{{{
--    as = 'deferring-diagnostics',
--    config = function() require('dd').setup() end
-- } --}}}

----------------------------------------------------------------------}}}

--                           Treesitter                               {{{
-------------------------------------------------------------------------
use { 'nvim-treesitter/nvim-treesitter', as = 'treesitter',
   requires = {
      {'nvim-treesitter/nvim-treesitter-textobjects', as = 'treesitter-textobjects'},
      {'RRethy/nvim-treesitter-textsubjects',         as = 'treesitter-textsubjects'},
      {'nvim-treesitter/nvim-treesitter-refactor',    as = 'treesitter-refactor'},
      -- {'romgrk/nvim-treesitter-context',              as = 'treesitter-context'},
      {'JoosepAlviste/nvim-ts-context-commentstring', as = 'treesitter-context-commentstring'},
      {'nvim-treesitter/playground',                  as = 'treesitter-playground'},
      {'p00f/nvim-ts-rainbow',                        as = 'treesitter-rainbow'},
   },
   run = ':TSUpdate',
   config = function() require('plugins-config/treesitter') end
}
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
   config = function() require('plugins-config/luasnip') end,
}

-- The community driven  collection of snippets.
-- use { 'rafamadriz/friendly-snippets' }

use { 'hrsh7th/nvim-cmp', as = 'cmp',
   config = function() require('plugins-config/cmp') end,
   requires = {
      {  -- Icons in completion menu.
         'onsails/lspkind-nvim', as = 'lspkind',
         config = function() require('plugins-config/lspkind') end
      },
      'saadparwaiz1/cmp_luasnip', -- source for LuaSnip snippet plugin
      'hrsh7th/cmp-nvim-lsp', -- source for neovim's Lua runtime API such 'vim.lsp.*'
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      -- 'uga-rosa/cmp-dictionary'
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
--    setup  = function() require('plugins-config/coq_nvim').setup() end,
--    config = function() require('plugins-config/coq_nvim').config() end,
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
--    config = function() vim.cmd("source ~/.config/nvim/lua/plugins-config/wilder.vim") end
-- }
-- --}}}

----------------------------------------------------------------------}}}

--                  DAP (Debug Adapter Protocol)                      {{{
-------------------------------------------------------------------------
use { 'mfussenegger/nvim-dap', as = 'dap' }
-- use { 'rcarriga/nvim-dap-ui',  as = 'dap-ui' }
----------------------------------------------------------------------}}}

--                          Text editing                              {{{
-------------------------------------------------------------------------

-- Makes *-operator works the way it should by default.
use { 'haya14busa/vim-asterisk', as = 'asterisk', --{{{
   config = function()
      vim.g['asterisk#keeppos'] = 0 -- Keep cursor position inside word between jumps.
      require('keybindings').asterisks()
   end
} --}}}

-- use 'matze/vim-move'       -- перемещение строк и частей строк
use { 'booperlv/nvim-gomove', as = 'gomove', --{{{
   config = function() require("gomove").setup({
      -- Whether to not to move past last column when moving blocks
      -- horizontally. (true/false)
      move_past_end_col = true,
   }) end
} --}}}

use 'wellle/targets.vim'   -- plugin that provides additional text objects
use 'tpope/vim-unimpaired' -- Different bidirectional motions: switch
                           -- buffers, add blank lines, etc.

-- Comments {{{

-- -- Also define `gc` - comment textobject.
-- -- Doesn't work in visual mode. Works in operator mode (:help mapmode-o).
-- use { 'echasnovski/mini.nvim', --{{{
--    config = function()
--       require('mini.comment').setup{}
--       --
--       require('which-key').register {
--          gc = { name = 'Comment' },
--          gcc = 'Comment out current line',
--          ['gc*'] = "which_key_ignore",
--          ['gc#'] = "which_key_ignore",
--       }
--    end
-- } --}}}

use 'tomtom/tcomment_vim'
use { 'junegunn/vim-easy-align', as = 'easy-align',
   config = function() require('keybindings').easy_align() end
}

-- }}}

-- Surround {{{

use { 'tpope/vim-surround', as = 'surround',
   requires = 'tpope/vim-repeat'
}

-- }}}

-- use { 'kevinhwang91/nvim-hlslens', as = 'hlslens',
--    config = function() require('plugins-config/hlslens') end
-- }

use { 'AndrewRadev/splitjoin.vim', as = 'splitjoin' }

-- -- The same as 'splitjoin', but in lua and may be some day will using treesitter.
-- use { 'AckslD/nvim-revJ.lua', --{{{
--    requires = 'wellle/targets.vim',
--    config = function() require("revj").setup{
--       keymaps = {
--           operator = '<Leader>J', -- for operator (+motion)
--           line = '<Leader>j', -- for formatting current line
--           visual = '<Leader>j', -- for formatting visual selection
--       },
--    } end
-- } --}}}

-- Multiple cursors
use { 'mg979/vim-visual-multi', as = 'multiple-cursors' }

----------------------------------------------------------------------}}}

--                      Clipboard and registers                       {{{
-------------------------------------------------------------------------

use { 'AckslD/nvim-neoclip.lua', --{{{
   requires = {
      { 'tami5/sqlite.lua', module = 'sqlite' },
   },
   config = function() require('neoclip').setup() end
} --}}}

-- use 'tversteeg/registers.nvim'

----------------------------------------------------------------------}}}

--                          Visual tweaks                             {{{
-------------------------------------------------------------------------

-- Highlight all other words the same as under the cursor.
use 'RRethy/vim-illuminate'

-- Indentation guides {{{
use { 'lukas-reineke/indent-blankline.nvim',
   ft = {
      'python',
      -- 'lua'
   },
   config = function() require("plugins-config/indent-blankline") end
}
--}}}

----------------------------------------------------------------------}}}

--                   Windows and buffers managment                    {{{
-------------------------------------------------------------------------

-- use 'matbme/JABS.nvim'

use { 'jlanzarotta/bufexplorer', --{{{
   -- requires = 'ryanoasis/vim-devicons', -- Install to enable devicons.
   config = function()
      vim.g.bufExplorerDisableDefaultKeyMapping = 1 -- Disable default mappings.
      vim.g.bufExplorerFindActive = 0  -- Do not go to active window.
      vim.g.bufExplorerShowNoName = 1  -- Show "No Name" buffers.
      vim.g.bufExplorerShowRelativePath = 1 -- Show relative paths.
   end
} --}}}

-- -- Broken friendship with barbar-tabline
-- use { 'vim-ctrlspace/vim-ctrlspace', --{{{
--    config = function ()
--       vim.g.CtrlSpaceDefaultMappingKey = "<C-space> "
--    end
-- } --}}}

use { 'roxma/vim-window-resize-easy', as = 'window-resize-easy' }

-- use { 'simeji/winresizer', --{{{
--   config = function()
--      vim.g.winresizer_vert_resize  = 1
--      vim.g.winresizer_horiz_resize = 1
--      -- vim.g.winresizer_start_key = '<leader>w'
--      vim.keymap.set('n', '<leader>w', '<cmd>WinResizerStartResize<CR>', { desc = 'Window resize mode', silent = true })
--   end
-- } --}}}

-- use{ 'mrjones2014/smart-splits.nvim', as = 'smart-splits', --{{{
--    config = function()
--       -- resizing splits
--       vim.keymap.set('n', '<A-h>', require('smart-splits').resize_left)
--       vim.keymap.set('n', '<A-j>', require('smart-splits').resize_down)
--       vim.keymap.set('n', '<A-k>', require('smart-splits').resize_up)
--       vim.keymap.set('n', '<A-l>', require('smart-splits').resize_right)
--       -- moving between splits
--       vim.keymap.set('n', '<C-h>', require('smart-splits').move_cursor_left)
--       vim.keymap.set('n', '<C-j>', require('smart-splits').move_cursor_down)
--       vim.keymap.set('n', '<C-k>', require('smart-splits').move_cursor_up)
--       vim.keymap.set('n', '<C-l>', require('smart-splits').move_cursor_right)
--    end
-- } --}}}

use { 'beauwilliams/focus.nvim', --{{{
   -- cmd = { "FocusSplitNicely", "FocusSplitCycle" }, module = "focus",
   config = function() require('focus').setup {
      -- Prevents focus automatically resizing windows based on configured
      -- excluded filetypes or buftypes.
      excluded_filetypes = { 'toggleterm', 'qf' },
      excluded_buftypes = {
         'quickfix', 'nofile', 'prompt', 'popup', -- Default, should always be.
      },
      -- Enable resizing for excluded filetypes using forced_filetypes.
      forced_filetypes = {}, -- 'dan_repl'
      --
      -- Displays line numbers in the focussed window only and
      -- not display in unfocussed windows.
      signcolumn = false,
      number = false,
      cursorline = false,
      cursorcolumn = false
   } end
} --}}}

----------------------------------------------------------------------}}}

--                            Movements                               {{{
-------------------------------------------------------------------------

-- Make vim treat all word delimiters like it treats spaces (for word motions).
-- use 'kana/vim-smartword'
use '~/code/neovim-plugins/vim-smartword'


-- use { 'easymotion/vim-easymotion', as = 'easymotion', --{{{
--    config = function()
--       vim.cmd("source ~/.config/nvim/lua/plugins-config/easymotion.vim")
--    end
-- } --}}}

use { 'phaazon/hop.nvim', as = 'easymotion-hop', --{{{
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
} --}}}

-- use {'ggandor/lightspeed.nvim', }

--                          Smooth scroll                             {{{
-------------------------------------------------------------------------

-- use { 'psliwka/vim-smoothie', --{{{
--    config = function()
--       -- Time (in milliseconds) between subseqent screen/cursor postion
--       -- updates.  Lower value produces smoother animation.
--       vim.g.smoothie_update_interval = 20
--
--       -- Base scrolling speed (in lines per second), to be taken into account
--       -- by the velocity calculation algorithm.  Can be decreased to achieve
--       -- slower (and easier to follow) animation.
--       vim.g.smoothie_base_speed = 7
--    end
-- } --}}}

use { 'karb94/neoscroll.nvim',
   config = function() require('plugins-config/neoscroll') end
}

----------------------------------------------------------------------}}}

-- -- Clever-f
-- use { 'rhysd/clever-f.vim', --{{{
--    config = function()
--       vim.g.clever_f_ignore_case = 1
--       vim.g.clever_f_smart_case = 1
--       vim.g.clever_f_show_prompt = 1
--       vim.g.clever_f_chars_match_any_signs = ';'
--    end
-- } --}}}

-- use 'chaoren/vim-wordmotion'  -- More useful word motions for Vim

----------------------------------------------------------------------}}}

--                           IDE features                             {{{
-------------------------------------------------------------------------

use { 'ahmedkhalf/project.nvim', --{{{
   config = function() require('project_nvim').setup {
      detection_methods = { 'lsp', 'pattern' },
      patterns = {
         '.git', '_darcs', '.hg', '.bzr', '.svn', 'Makefile', 'package.json',
         '>.config', '>roles'
      },
      -- When set to false, you will get a message
      -- when project.nvim changes your directory.
      silent_chdir = true,
   } end
} --}}}

use { 'klen/nvim-config-local', as = 'per-project-config',
   config = function()
      require('config-local').setup {
         -- Config file patterns to load (lua supported)
          config_files = { '.vimrc.lua', '.vimrc' },
          autocommands_create = true, -- Create autocommands (VimEnter, DirectoryChanged)
          commands_create = false, -- Create commands (ConfigSource, ConfigEdit, ConfigTrust, ConfigIgnore)
          silent = false, -- Disable plugin messages (Config loaded/ignored)
          lookup_parents = true,-- Lookup config files in parent directories
      }
   end
}

-- use { 'sidebar-nvim/sidebar.nvim',
--    config = function() require('plugins-config/sidebar') end
-- }

-- Litee {{{

use { 'ldelossa/litee.nvim',
   config = function() require('litee.lib').setup({
      tree = {
         icon_set = "codicons"
      },
      -- panel = {
      --    orientation = 'right',
      --    panel_size  = 30
      -- }
   }) end
}

use { 'ldelossa/litee-calltree.nvim',
   config = function() require('litee.calltree').setup() end
}

-- use { 'ldelossa/litee-symboltree.nvim',
--    config = function() require('litee.symboltree').setup({
--       icon_set = "codicons",
--       -- on_open = 'panel', -- "panel" or "popout"
--    }) end
-- }

-- use { 'ldelossa/litee-bookmarks.nvim',
--    config = function() require('litee.bookmarks').setup({}) end
-- }

-- }}}

-- use 'tpope/vim-apathy'  -- Make 'gf' keybinding work in different filetypes.
--                         -- For instructions of what it can do more look:
--                         -- https://github.com/tpope/vim-apathy
--                         -- and
--                         -- :help include-search

-- Asynchronous build and test dispatcher
use { 'tpope/vim-dispatch',
   cmd = { 'Dispatch', 'Make', 'Focus', 'Start' }
}

use { 'TimUntersberger/neogit', --{{{
   requires = 'nvim-lua/plenary.nvim',
   config = function() require('neogit').setup{} end
} --}}}

use { 'lewis6991/gitsigns.nvim', as = 'gitsigns', -- {{{
   requires = 'tpope/vim-repeat',
   config = function()
      require('gitsigns').setup {
         signcolumn   = false, -- Toggle with ":Gitsigns toggle_signs"
         numhl        = false, -- :Gitsigns toggle_numhl
         linehl       = false, -- :Gitsigns toggle_linehl
         word_diff    = false, -- :Gitsigns toggle_word_diff
         on_attach = require('keybindings').gitsigns
      }
   end
} --}}}
----------------------------------------------------------------------}}}

--------------------------- Fuzzy finder --------------------------------
use { 'nvim-telescope/telescope.nvim', as = 'telescope', --{{{
   requires = {
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', as = 'telescope-fzf-native',
         run = 'make'
      },
      { "natecraddock/telescope-zf-native.nvim", as = 'telescope-zf-native' },
      -- { "nvim-telescope/telescope-frecency.nvim", as = 'telescope-frecency',
      --    requires = "tami5/sqlite.lua"
      -- },
      { 'jvgrootveld/telescope-zoxide',
         requires = 'nvim-lua/popup.nvim'
      }
      -- 'famiu/bufdelete.nvim',
   },
   config = function() require('plugins-config/telescope') end,
} --}}}
-------------------------------------------------------------------------

--                      Vim additional modules                        {{{
-------------------------------------------------------------------------

-- use 'yegappan/mru'

use { 'stevearc/dressing.nvim', --{{{
   -- requires = 'MunifTanjim/nui.nvim',
   config = function() require('plugins-config.dressing') end
} --}}}

-- -- Not work in Neovim 0.7 yet.
-- use { 'filipdutescu/renamer.nvim', as = 'renamer', branch = 'master', --{{{
--    requires = 'nvim-lua/plenary.nvim',
--    config = function()
--       require('renamer').setup {}
--    end
-- } --}}}

-- use { 'anuvyklack/pretty-fold.nvim', as = 'pretty-fold', -- {{{
use { '~/code/neovim-plugins/pretty-fold.nvim', as = 'pretty-fold',
   requires = 'keymap-amend',
   config = function() require('plugins-config/pretty-fold') end
} --}}}

use { 'sindrets/diffview.nvim',
   requires = 'nvim-lua/plenary.nvim'
}

-- Marks
-- use 'kshenoy/vim-signature'  -- display and navigate marks
use { 'chentoast/marks.nvim', --{{{
   config = function()
      require'marks'.setup {
         -- https://github.com/chentau/marks.nvim/issues/40
         -- 0 or some really large value to disable mark tracking
         refresh_interval = 0,

         default_mappings = true,
         -- builtin_marks = { ".", "<", ">", "^", "'" }, -- which builtin marks to show
         signs = true,
         excluded_filetypes = { 'gitcommit' },
         mappings = {}
      }
      -- vim.keymap.set('n', '<leader>fm', '<Cmd>MarksListBuf<CR>', { desc = 'Marks' })
   end
} --}}}

-- Visualize undo tree
-- -- use 'simnalamburt/vim-mundo'  -- another undo tree visualizer
-- use { 'mbbill/undotree', --{{{
--    config = function()
--       vim.g.undotree_HighlightChangedWithSign = 0
--       vim.g.undotree_WindowLayout = 2
--    end
-- } --}}}

use { 'kevinhwang91/nvim-bqf', as = 'better-quickfix' }

use { 'https://gitlab.com/yorickpeterse/nvim-pqf', as = 'pretty-quickfix', --{{{
   config = function() require('pqf').setup {
      -- signs = {
      --    error = 'E',
      --    warning = 'W',
      --    info = 'I',
      --    hint = 'H'
      -- }
   } end
} --}}}

-- <leader>? : 40-column cheat sheet
use { 'anuvyklack/vim-cheat40', as = 'cheat40' } -- my fork
-- use { 'sudormrfbin/cheatsheet.nvim', --{{{
--    requires = {
--       -- { 'nvim-telescope/telescope.nvim', as = 'telescope' },
--       'nvim-lua/popup.nvim',
--       'nvim-lua/plenary.nvim',
--    },
--    config = function()
--       require("cheatsheet").setup({})
--    end
-- } --}}}

-- -- TODO Need to rewrite in lua.
-- use { 'camspiers/lens.vim', --{{{
--    requires = 'camspiers/animate.vim',
--    config = function() vim.cmd("source ~/.config/nvim/lua/plugins-config/lens.vim") end
-- } --}}}

use { 'norcalli/nvim-colorizer.lua', as = 'colorizer', --{{{
   ft = {'vim', 'lua', 'conf', 'tmux', 'kitty', 'vifm', 'markdown', 'zsh'},
   config = function() require'colorizer'.setup {
      'markdown'
   } end
} --}}}

use { 'anuvyklack/help-vsplit.nvim', as = 'help-vsplit', --{{{
-- use { '~/Git/my_neovim-plugins/help-vsplit.nvim', as = 'help-vsplit',
   config = function() require('help-vsplit').setup {
      always = true,
      side = 'left'
   } end
} --}}}

-- Execute :StartupTime to get an averaged startup profile.
use { 'tweekmonster/startuptime.vim', as = 'startuptime',
   cmd = 'StartupTime'
}

----------------------------------------------------------------------}}}

--                              Syntax                                {{{
-------------------------------------------------------------------------

-- Show syntax highlighting attributes of character under cursor.
use 'vim-scripts/SyntaxAttr.vim'

-- -- Automatically adjusts 'shiftwidth' and 'expandtab' heuristically
-- -- based on the current file.
-- use { 'tpope/vim-sleuth', --{{{
--    cmd = { 'Sleuth', 'verbose Sleuth' },
--    config = function()
--       -- Turn off automatic detection, потому что
--       -- думает за меня и думает лишнего.
--       vim.g.sleuth_automatic = 0
--    end
-- } --}}}

-- -- Подсветка синтаксисов для разных языков.
-- use { 'sheerun/vim-polyglot', --{{{
--    setup = function()  -- run before plugin load
--       -- This variable should be declared before polyglot is loaded!
--       vim.g.polyglot_disabled = {
--          'sensible',
--          'autoindent', -- use https://github.com/tpope/vim-sleuth instead
--          'help',  -- because it force set 'expandtab'
--          'norg',
--          -- 'markdown',
--          -- 'gitignore',
--          -- 'txt',
--       }
--    end
-- } --}}}

use 'pearofducks/ansible-vim'

use { 'Neui/cmakecache-syntax.vim', as = 'syntax-cmakecache' }
-- use { 'zinit-zsh/zinit-vim-syntax', ft = 'zsh' } -- zinit syntaxis

use { 'anuvyklack/vim-dealii-prm', as = 'syntax-dealii-prm',
   -- event = 'BufNewFile,BufRead *.prm'
}

-- Kitty terminal conf file syntax highlight.
use { "fladson/vim-kitty", as = 'syntax-kitty-conf' }

----------------------------------------------------------------------}}}

--                           File manager                             {{{
-------------------------------------------------------------------------

-- use { 'kyazdani42/nvim-tree.lua',
--    requires = 'kyazdani42/nvim-web-devicons',
--    config = function() require('plugins-config/nvim-tree') end
-- }

use { "nvim-neo-tree/neo-tree.nvim",
   branch = "v2.x",
   requires = {
      'nvim-lua/plenary.nvim',
      'kyazdani42/nvim-web-devicons',
      'MunifTanjim/nui.nvim'
   },
   config = function() require('plugins-config/neo-tree') end
}

use { 'elihunter173/dirbuf.nvim', --{{{
   config = function()
     require('dirbuf').setup {
        sort_order = "directories_first",
        show_hidden = false,
        hash_padding = 2,
     }
   end
} --}}}

-- use { 'tamago324/lir.nvim', as = 'lir-filemanager', --{{{
--    requires = {
--       'nvim-lua/plenary.nvim',
--       'kyazdani42/nvim-web-devicons',
--    },
--    config = function() require('plugins-config/lir') end
-- } --}}}

use { 'mcchrish/nnn.vim',
   config = function() require('plugins-config/nnn-vim') end
}

-- use { 'luukvbaal/nnn.nvim',
--    config = function() require('plugins-config/nnn-nvim') end
-- }

-- other {{{

use { 'vifm/vifm.vim',
   -- TODO Open issue to packer: help tags not generated automaticaly.
   -- run = ':helptags ALL',
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

-- use { 'is0n/fm-nvim',
--    config = function() require('plugins-config/fm-nvim') end
-- }

-- }}}

----------------------------------------------------------------------}}}

--                      Lua plugins development                       {{{
-------------------------------------------------------------------------
use { 'rafcamlet/nvim-luapad', as = 'luapad',
   config = function()
      local command = vim.api.nvim_create_user_command
      local luapad = require('luapad')

      command('LuaAttach', luapad.attach, { bang = true })
      command('LuaDetach', luapad.detach, { bang = true })
      command('LuaToggle', luapad.toggle, { bang = true })

      vim.api.nvim_create_autocmd('BufEnter', {
         pattern = '*_Luapad.lua',
         command = 'setlocal bufhidden=hide'
      })
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

use { 'nvim-neorg/neorg',
   requires = "nvim-lua/plenary.nvim",
   after = "treesitter",
   config = function() require('plugins-config/neorg') end
}

----------------------------------------------------------------------}}}

--                         Tmux integration                           {{{
-------------------------------------------------------------------------
use 'tmux-plugins/vim-tmux' -- tmux.conf syntax

use { 'anuvyklack/vim-tmux-navigator', -- my fork
   config = function()
      -- Activate autoupdate on exit.
      vim.g.tmux_navigator_save_on_switch = 0

      -- Disable vim->tmux navigation when the Vim pane is zoomed in tmux.
      vim.g.tmux_navigator_disable_when_zoomed = 1
   end
}
----------------------------------------------------------------------}}}

--                              Pandoc                                {{{
-------------------------------------------------------------------------
-- use { 'vim-pandoc/vim-pandoc',
--    requires = {'vim-pandoc/vim-pandoc-syntax', opt = true},
--    ft = {'markdown', 'pandoc'},
--    config = function()
--       vim.cmd("source ~/.config/nvim/lua/plugins-config/pandoc.vim")
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
      --
      -- require('which-key').register {
      --    gh = 'which_key_ignore',
      --    gH = 'test description',
      --    ['g<C-H>'] = 'which_key_ignore'
      -- }
   end
}
-- use 'powerman/vim-plugin-ruscmd'
----------------------------------------------------------------------}}}

--                        Statusline, Tabline                         {{{
-------------------------------------------------------------------------

use { 'romgrk/barbar.nvim', as = 'barbar-tabline', --{{{
   requires = 'kyazdani42/nvim-web-devicons',
   config = function() require('plugins-config/barbar') end
} --}}}

use { 'SmiteshP/nvim-gps',
   config = function()
      require("nvim-gps").setup{
         icons = {
            -- ["class-name"] = ' ',     --  Classes and class-like objects
            ["class-name"] = ' ',     --  Classes and class-like objects
            ["function-name"] = ' ',  -- Functions
            ["method-name"] = '  ',   -- Methods (functions inside class-like objects)
            ["container-name"] = ' ', -- Containers (example: lua tables)
            ["tag-name"] = '炙'        -- Tags (example: html tags)
         },
      }
   end
}

use { 'rebelot/heirline.nvim',
   requires = 'gitsigns',
   after = colorscheme,
   config = function()
      vim.o.showmode = false
      require('plugins-config/heirline')
   end
}

----------------------------------------------------------------------}}}

return setmetatable({}, {
   __index = function(_, key)
      return packer[key]
   end,
})

-- vim: fml=1 fdm=marker
