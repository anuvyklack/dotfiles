--         ███                 ██                    ███
--        ░░██                ░░                    ░░██
--  ██████ ░██ ██   ██  ██████ ██ ██████   ██████    ░██ ██   ██  █████
-- ░██░░░██░██░██  ░██ ██░░░██░██░██░░░██ ██░░░░     ░██░██  ░██ ░░░░░██
-- ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░█████     ░██░██  ░██  ██████
-- ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██    ░██░██  ░██ ██░░░██
-- ░██░░░  ░██░░█████  ░░░░░██░██░██  ░██ ██████  ██ ░██░░█████ ░░███████
-- ░██     ░░  ░░░░░    █████ ░░ ░░   ░░ ░░░░░░  ░░  ░░  ░░░░░   ░░░░░░░
-- ░░                  ░░░░░

-- Init Packer ------------------------------------------------------------- {{{
local packer = require('packer')
local use = packer.use
local join_paths = require('packer.util').join_paths

packer.init { --{{{
   compile_path = join_paths(vim.fn.stdpath('config'), 'plugin', '1_packer_compiled.lua'),
   display = {
      open_cmd = '90vnew [packer]', -- set the width of the packer split
      -- open_fn = function()
      --    return require("packer.util").float({
      --       border = "rounded",
      --    })
      -- end,
      working_sym = '',
      error_sym = '',
      done_sym = '',
   },
   git = {
      clone_timeout = 600 -- timeout, in seconds, for git clones
   }
} --}}}
---------------------------------------------------------------------------- }}}

use { 'wbthomason/packer.nvim', opt = true }

use 'lewis6991/impatient.nvim' -- improve startup time

use 'anuvyklack/middleclass'

-- use 'anuvyklack/nvim-api-wrappers'
use '~/code/neovim-plugins/nvim-api-wrappers'


-- Key Mapppings ----------------------------------------------------------- {{{

-- use { 'folke/which-key.nvim', -- original
--    as = 'which-key', -- WhichKey
--    config = function() require('anuvyklack/which-key') end
-- }

-- use { 'anuvyklack/nvim-keymap-amend', as = 'keymap-amend' }
use { '~/code/neovim-plugins/keymap-amend.nvim', as = 'keymap-amend' }

-- use 'anuvyklack/hydra.nvim'
-- use 'anuvyklack/keymap-layer.nvim'
use '~/code/neovim-plugins/hydra.nvim'
-- use '~/code/neovim-plugins/keymap-layer.nvim'

use 'tpope/vim-repeat'

-- }}}

-- Color Schemes ----------------------------------------------------------- {{{
use 'rktjmp/lush.nvim'
use 'sainnhe/gruvbox-material'
use 'savq/melange'
use 'karoliskoncevicius/moonshine-vim'
use { 'srcery-colors/srcery-vim', as = 'srcery' }
use { 'adigitoleo/vim-mellow', as = 'mellow' }
use 'folke/tokyonight.nvim'
use { 'sainnhe/everforest' }

-- My fork of vim-paper scheme.
-- https://gitlab.com/yorickpeterse/vim-paper.git
use '~/code/neovim-plugins/colorsheme/manuscript'

use { 'folke/lsp-colors.nvim', --{{{
   config = function()
      require('lsp-colors').setup {
         Error       = "#db4b4b",
         Warning     = "#e0af68",
         Information = "#0db9d7",
         Hint        = "#10B981"
      }
   end
} --}}}

-----------------------------------------------------------------------------}}}

-- Statusline, Tabline ----------------------------------------------------- {{{

-- use { '~/code/neovim-plugins/!git-repos/barbar.nvim',
use { 'romgrk/barbar.nvim', as = 'barbar-tabline',
   requires = 'kyazdani42/nvim-web-devicons',
   config = function()
      require('bufferline').setup {
         icon_pinned = ''
      }
   end
}

use { 'rebelot/heirline.nvim',
   config = function()
      vim.o.showmode = false
      require('anuvyklack/heirline')
   end
}

use { 'SmiteshP/nvim-navic', as = 'navic',
   config = function() require('anuvyklack/navic') end }

-----------------------------------------------------------------------------}}}

-- Treesitter -------------------------------------------------------------- {{{
use { 'nvim-treesitter/nvim-treesitter', as = 'treesitter',
   requires = {
      { 'nvim-treesitter/nvim-treesitter-textobjects', as = 'treesitter-textobjects' },
      { 'RRethy/nvim-treesitter-textsubjects', as = 'treesitter-textsubjects' },
      -- { 'nvim-treesitter/nvim-treesitter-refactor',    as = 'treesitter-refactor'},
      -- { 'romgrk/nvim-treesitter-context',              as = 'treesitter-context'},
      { 'JoosepAlviste/nvim-ts-context-commentstring', as = 'treesitter-context-commentstring' },
      { 'p00f/nvim-ts-rainbow', as = 'treesitter-rainbow' },
   },
   run = ':TSUpdate',
   config = function() require('anuvyklack/treesitter') end
}

use { 'mizlan/iswap.nvim',
   requires = 'treesitter',
   config = function()
      require('iswap').setup {
         autoswap = true
      }
      require('keymaps').iswap()
   end
}

use { 'nvim-treesitter/playground', as = 'treesitter-playground',
   requires = 'treesitter',
   cmd = { 'TSPlaygroundToggle', 'TSHighlightCapturesUnderCursor' }
}
--}}}

-- Completion -------------------------------------------------------------- {{{

-- Autopairs
use { 'windwp/nvim-autopairs', as = 'autopairs', -- {{{
   config = function()
      require('nvim-autopairs').setup {
         check_ts = true, -- check treesitter
         disable_in_macro = true, -- disable when recording or executing a macro
      }
   end
} -- }}}

use { 'hrsh7th/nvim-cmp', as = 'cmp', -- {{{
   requires = {
      { 'L3MON4D3/LuaSnip',
         requires = 'rafamadriz/friendly-snippets',
         config = function()
            require("luasnip.loaders.from_vscode").lazy_load()
            require('anuvyklack/luasnip')
            require('keymaps').luasnip()
         end },
      { 'onsails/lspkind-nvim', -- Icons in completion menu.
         config = function() require('anuvyklack/lspkind') end },
      'saadparwaiz1/cmp_luasnip', -- source for LuaSnip snippet plugin
      'hrsh7th/cmp-nvim-lsp', -- source for neovim's Lua runtime API such 'vim.lsp.*'
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      'hrsh7th/cmp-nvim-lua',
      -- 'uga-rosa/cmp-dictionary'
   },
   config = function() require('anuvyklack/cmp') end
} -- }}}

-- -- coq {{{
-- use { 'ms-jpq/coq_nvim', branch = 'coq',
--    after = 'autopairs',
--    requires = {
--       { 'ms-jpq/coq.artifacts', branch = 'artifacts' }, -- 9000+ Snippets
--       { 'ms-jpq/coq.thirdparty', branch = '3p' } -- https://github.com/ms-jpq/coq.thirdparty
--    },
--    setup  = function() require('anuvyklack/coq_nvim').setup() end,
--    config = function() require('anuvyklack/coq_nvim').config() end,
-- }
-- -- }}}

-- -- wilder {{{
-- use { 'gelguy/wilder.nvim',
--    requires = {
--       'roxma/nvim-yarp',
--       'roxma/vim-hug-neovim-rpc'
--    },
--    run = ':UpdateRemotePlugins',
--    event = 'CmdlineEnter',
--    config = function() vim.cmd 'source ~/.config/nvim/lua/anuvyklack/wilder.vim' end
-- }
-- -- }}}

-- }}}

-- LSP --------------------------------------------------------------------- {{{
use 'williamboman/mason.nvim'
use { 'folke/neodev.nvim', config = function() require 'anuvyklack/neodev' end }
use { 'neovim/nvim-lspconfig', as = 'lspconfig' }
use 'williamboman/mason-lspconfig.nvim'
use 'jose-elias-alvarez/null-ls.nvim'
use { 'ray-x/lsp_signature.nvim', config = function() require 'anuvyklack/lsp_signature' end }
use { 'j-hui/fidget.nvim', config = function() require 'anuvyklack/fidget' end }
use { 'lvimuser/lsp-inlayhints.nvim', config = function() require('lsp-inlayhints').setup() end }
use 'https://git.sr.ht/~p00f/clangd_extensions.nvim'

-- use 'RRethy/vim-illuminate' -- Highlight all other words the same as under the cursor

-- use { 'filipdutescu/renamer.nvim', as = 'renamer', branch = 'master', --{{{
--    requires = 'nvim-lua/plenary.nvim',
--    config = function()
--       require('renamer').setup {}
--    end
-- } --}}}

-- use { 'rmagatti/goto-preview', -- {{{
--    config = function()
--       require('goto-preview').setup {
--          default_mappings = true,
--          post_open_hook = function(bufnr, winnr)
--             vim.api.nvim_create_autocmd('WinLeave', {
--                callback = function() vim.api.nvim_win_close(winnr, false) end,
--                buffer = bufnr,
--                once = true
--             })
--             vim.keymap.set('n', '<Esc>', function()
--                vim.api.nvim_win_close(winnr, false)
--             end, { buffer = bufnr })
--          end
--       }
--    end
-- } -- }}}

-- use { 'kosayoda/nvim-lightbulb', as = 'lightbulb', --{{{
--    config = function() require 'anuvyklack/lightbulb' end
-- } --}}}

-- use { 'weilbith/nvim-code-action-menu', as = 'code-action-menu',
--    cmd = 'CodeActionMenu' }

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

use { 'https://git.sr.ht/~whynothugo/lsp_lines.nvim', -- {{{
   config = function()
      require('lsp_lines').setup()
      vim.diagnostic.config({
         underline = false,
         virtual_text = true,
         virtual_lines = false,
      })
   end,
} -- }}}

-- use { "narutoxy/dim.lua", --{{{
--   config = function() require('dim').setup({}) end
-- } --}}}

-- use 'jubnzv/virtual-types.nvim'

-- use { 'folke/trouble.nvim', --{{{
--    requires = 'kyazdani42/nvim-web-devicons',
--    -- cmd = { 'Trouble', 'TroubleToggle' },
--    config = function() require 'anuvyklack/trouble' end
-- } --}}}

use { 'liuchengxu/vista.vim',
   config = function() vim.cmd 'source ~/.config/nvim/lua/anuvyklack/vista.vim' end }

-- use { 'simrat39/symbols-outline.nvim', as = 'symbols-outline', --{{{
--    config = function()
--       require('anuvyklack/symbols-outline')
--       vim.cmd 'source ~/.config/nvim/lua/anuvyklack/symbols-outline.vim'
--    end
-- } --}}}

-- use { 'stevearc/aerial.nvim', as = 'aerial', --{{{
--    config = function() require("aerial").setup({
--       show_guides = false,
--    }) end
-- } --}}}

use { 'https://gitlab.com/yorickpeterse/nvim-dd.git', as = 'deferring-diagnostics',
   config = function()
      require('dd').setup {
         timeout = 1000 -- The time to wait before displaying newly produced diagnostics.
      }
   end
}

--}}}

-- Debugging --------------------------------------------------------------- {{{

-- use { "puremourning/vimspector",
--    setup = function()
--       vim.fn.sign_define('vimspectorBP', { text = ' ●', texthl = 'VimspectorBreakpoint' })
--       vim.fn.sign_define('vimspectorBPCond', { text = ' ●', texthl = 'VimspectorBreakpointCond' })
--       vim.fn.sign_define('vimspectorBPDisabled', { text = ' ●', texthl = 'VimspectorBreakpointDisabled' })
--       vim.fn.sign_define(
--          'vimspectorPC',
--          { text = '▶', texthl = 'VimspectorProgramCounter', linehl = 'VimspectorProgramCounterLine' }
--       )
--       vim.fn.sign_define('vimspectorPCBP', {
--          text = '●▶',
--          texthl = 'VimspectorProgramCounterBreakpoint',
--          linehl = 'VimspectorProgramCounterLine',
--       })
--    end,
--    ft = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact' },
-- }

-- use { 'mfussenegger/nvim-dap', as = 'dap',
--    requires = { 'rcarriga/nvim-dap-ui', as = 'dap-ui' }
-- }

-- }}}

-- Windows and buffers managment ------------------------------------------- {{{

-- use { 'anuvyklack/windows.nvim',
--    requires = 'anuvyklack/animation.nvim',
use { '~/code/neovim-plugins/windows.nvim',
   requires = '~/code/neovim-plugins/animation.nvim',
   config = function()
      vim.o.winwidth = 10
      vim.o.winminwidth = 10
      vim.o.equalalways = false

      require('windows').setup {
         autowidth = {
            -- enable = false,
            winwidth = 5,
            filetype = { help = 3 }
         },
         animation = {
            -- enable = false,
            -- duration = 250,
         }
      }
   end
}

use { 'folke/twilight.nvim',
   config = function()
      require('twilight').setup {
         context = 20
      }
   end
}

use { 'Pocco81/true-zen.nvim',
   config = function()
      require('true-zen').setup {
      }
   end
}

use { 'jlanzarotta/bufexplorer', --{{{
   -- requires = 'ryanoasis/vim-devicons', -- Install to enable devicons.
   config = function()
      vim.g.bufExplorerDisableDefaultKeyMapping = 1 -- Disable default mappings.
      vim.g.bufExplorerFindActive = 0 -- Do not go to active window.
      vim.g.bufExplorerShowNoName = 1 -- Show "No Name" buffers.
      vim.g.bufExplorerShowRelativePath = 1 -- Show relative paths.
      vim.g.bufExplorerSortBy = 'fullpath'
   end
} --}}}

-- use { 'ghillb/cybu.nvim',
--   branch = 'main',
--   requires = 'kyazdani42/nvim-web-devicons',
--   config = function()
--     require('cybu').setup()
--     vim.keymap.set('n', 'K', '<Plug>(CybuPrev)')
--     vim.keymap.set('n', 'J', '<Plug>(CybuNext)')
--     vim.keymap.set({'n','v'}, '<C-S-Tab>', '<Plug>(CybuLastusedPrev)')
--     vim.keymap.set({'n','v'}, '<C-Tab>', '<Plug>(CybuLastusedNext)')
--   end,
-- }

use 'sindrets/winshift.nvim'
use 'mrjones2014/smart-splits.nvim'
use { 'https://gitlab.com/yorickpeterse/nvim-window.git' }

use { 'luukvbaal/stabilize.nvim',
   config = function() require('stabilize').setup() end
}

-- use { 'beauwilliams/focus.nvim', --{{{
--    -- cmd = { "FocusSplitNicely", "FocusSplitCycle" }, module = "focus",
--    config = function() require('focus').setup {
--       -- Prevents focus automatically resizing windows based on configured
--       -- excluded filetypes or buftypes.
--       excluded_filetypes = { 'toggleterm', 'qf' },
--       excluded_buftypes = {
--          'quickfix', 'nofile', 'prompt', 'popup', -- Default, should always be.
--       },
--       -- Enable resizing for excluded filetypes using forced_filetypes.
--       forced_filetypes = {}, -- 'dan_repl'
--       --
--       -- Displays line numbers in the focussed window only and
--       -- not display in unfocussed windows.
--       signcolumn = false,
--       number = false,
--       cursorline = false,
--       cursorcolumn = false
--    } end
-- } --}}}

-- -- Broken friendship with barbar-tabline
-- use { 'vim-ctrlspace/vim-ctrlspace', --{{{
--    config = function ()
--       vim.g.CtrlSpaceDefaultMappingKey = "<C-space> "
--    end
-- } --}}}

--}}}

-- Smooth scroll ----------------------------------------------------------- {{{

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

use { 'karb94/neoscroll.nvim', config = function() require('anuvyklack/neoscroll') end }

--}}}

-- Motions ----------------------------------------------------------------- {{{

-- Make vim treat all word delimiters like it treats spaces (for word motions).
-- use 'kana/vim-smartword'
use 'anuvyklack/vim-smartword' -- fork
-- use '~/code/neovim-plugins/quickword.nvim'

use 'chaoren/vim-wordmotion' -- More useful word motions for Vim

-- use { 'easymotion/vim-easymotion', as = 'easymotion',
--    config = function() vim.cmd('source ~/.config/nvim/lua/anuvyklack/easymotion.vim') end }

use { 'phaazon/hop.nvim', -- {{{
   config = function()
      require 'hop'.setup {
         winblend = 30,
         -- keys = 'asdghklqwertyuiopzxcvbnmfj'
         -- keys = 'asdghklqwertyuiopzxcvfjbnm'
         keys = 'asdghklqwertyuiopzxcvfjbn',
         uppercase_labels = false, -- Display labels as uppercase.
      }
      require('keymaps').hop()
   end
} -- }}}

-- use { 'ggandor/leap.nvim',
--    config = function() require('leap').set_default_keymaps() end }

-- use 'ggandor/lightspeed.nvim'

use { 'rhysd/clever-f.vim', --{{{
   config = function()
      local g = vim.g
      g.clever_f_ignore_case = 1
      g.clever_f_smart_case = 1
      g.clever_f_show_prompt = 1
      g.clever_f_chars_match_any_signs = ';'
   end
} --}}}

--}}}

-- Multiple cursors
use { 'mg979/vim-visual-multi', as = 'multiple-cursors' }

-- Yank ring --------------------------------------------------------------- {{{

-- use { '~/code/neovim-plugins/YankRing/YankRing.vim',
--    setup = function()
--       vim.g.yankring_clipboard_monitor = 0
--    end
-- }

use { 'gbprod/yanky.nvim',
   after = 'multiple-cursors',
   config = function()
      vim.o.clipboard = 'unnamed'
      require('yanky').setup {
         -- ring = {
         --    storage = 'shada',
         -- },
         -- system_clipboard = {
         --    sync_with_ring = true,
         -- },
         -- highlight = {
         --    on_put = true,
         --    on_yank = true,
         --    timer = 500,
         -- },
      }

      -- vim.cmd 'highlight link YankyYanked IncSearch'
      require('keymaps').yanky()
   end
}

-- use { 'AckslD/nvim-neoclip.lua', --{{{
--    requires = { { 'tami5/sqlite.lua', module = 'sqlite' }, },
--    config = function() require('neoclip').setup() end
-- } --}}}

-- }}}

-- Telescope --------------------------------------------------------------- {{{
use { 'nvim-telescope/telescope.nvim', as = 'telescope',
   after = 'yanky.nvim',
   requires = {
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' },
      'natecraddock/telescope-zf-native.nvim',
      'cljoly/telescope-repo.nvim',
      -- { 'nvim-telescope/telescope-frecency.nvim', requires = 'tami5/sqlite.lua' },
      -- 'famiu/bufdelete.nvim',
   },
   config = function()
      require 'anuvyklack/telescope'
      require('keymaps').telescope()
   end
   -- config = function()
   --    require('telescope').setup{}
   --    -- require('keymaps').telescope()
   -- end
}

-- use 'yegappan/mru'

-- use 'tversteeg/registers.nvim'

--}}}

-- Visual tweaks ----------------------------------------------------------- {{{

-- use { '~/code/neovim-plugins/pretty-fold.nvim', as = 'pretty-fold', --{{{
--    -- use { 'anuvyklack/pretty-fold.nvim', as = 'pretty-fold',
--    requires = 'keymap-amend',
--    config = function()
--       require('anuvyklack/pretty-fold')
--    end
-- } --}}}

use { 'kevinhwang91/nvim-ufo', as = 'ufo',
   requires = 'kevinhwang91/promise-async',
   config = function() require('anuvyklack/ufo') end
}

use { '~/code/neovim-plugins/fold-preview.nvim',
   requires = 'keymap-amend',
   config = function() require('fold-preview').setup() end
}

-- use { 'rcarriga/nvim-notify',
--    config = function() vim.notify = require("notify") end }

-- Indentation guides {{{
use { 'lukas-reineke/indent-blankline.nvim',
   ft = {
      'python',
      -- 'lua'
   },
   config = function() require("anuvyklack/indent-blankline") end
}
--}}}

-- use { 'lewis6991/satellite.nvim',
--   config = function() require('satellite').setup() end }

use { 'NvChad/nvim-colorizer.lua', as = 'colorizer', --{{{
   ft = { 'vim', 'lua', 'conf', 'tmux', 'kitty', 'vifm', 'markdown', 'zsh' },
   config = function()
      require 'colorizer'.setup {
         -- 'markdown'
      }
   end
} --}}}

--}}}

-- File manager ------------------------------------------------------------ {{{

-- use { 'kyazdani42/nvim-tree.lua',
use { '~/code/neovim-plugins/nvim-tree.lua',
   requires = 'kyazdani42/nvim-web-devicons',
   config = function() require('anuvyklack/nvim-tree') end
}

-- use { 'nvim-neo-tree/neo-tree.nvim',
--    branch = 'v2.x',
--    requires = {
--       'nvim-lua/plenary.nvim',
--       'kyazdani42/nvim-web-devicons',
--       'MunifTanjim/nui.nvim'
--    },
--    config = function() require('anuvyklack/neo-tree') end
-- }

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
--    config = function() require('anuvyklack/lir') end
-- } --}}}

-- use { 'mcchrish/nnn.vim',
--    config = function() require('anuvyklack/nnn-vim') end
-- }

-- use { 'luukvbaal/nnn.nvim',
--    config = function() require('anuvyklack/nnn-nvim') end
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
--    config = function() require('anuvyklack/fm-nvim') end
-- }

--}}}

--}}}

-- Git --------------------------------------------------------------------- {{{
use { 'lewis6991/gitsigns.nvim', as = 'gitsigns', --{{{
   requires = 'tpope/vim-repeat',
   config = function()
      require('gitsigns').setup {
         signcolumn = false, -- Toggle with ":Gitsigns toggle_signs"
         numhl      = false, -- :Gitsigns toggle_numhl
         linehl     = false, -- :Gitsigns toggle_linehl
         word_diff  = false, -- :Gitsigns toggle_word_diff
         on_attach  = require('keymaps').gitsigns
      }
   end
} --}}}

use { 'TimUntersberger/neogit', --{{{
   requires = 'nvim-lua/plenary.nvim',
   config = function() require('neogit').setup() end
} --}}}
---------------------------------------------------------------------------- }}}

-- Terminal ---------------------------------------------------------------- {{{

-- use { 'akinsho/toggleterm.nvim',
--    config = function()
--       require("toggleterm").setup {
--          insert_mappings = false,
--          env = {
--             MANPAGER = "less -X",
--          },
--          terminal_mappings = false,
--          start_in_insert = false,
--          open_mapping = [[<space>t]],
--          highlights = {
--             CursorLineSign = { link = "DarkenedPanel" },
--             Normal = { guibg = "#14141A" },
--          },
--       }
--
--       -- Remove WinEnter to allow moving a toggleterm to new tab
--       vim.cmd [[autocmd! ToggleTermCommands WinEnter]]
--    end,
-- }

---------------------------------------------------------------------------- }}}

-- Tests ------------------------------------------------------------------- {{{

-- use { 'janko/vim-test',
--    config = function()
--       vim.g['test#strategy'] = 'neovim'
--       vim.g['test#neovim#term_position'] = 'vsplit'
--       -- vim.keymap.set('n', '<leader>r', '<Cmd>TestNearest<CR>')
--    end
-- }

---------------------------------------------------------------------------- }}}

use { 'echasnovski/mini.nvim', --{{{
   config = function()
      -- Also define `gc` - comment textobject. Works in operator mode (:help mapmode-o).
      require('mini.comment').setup {}

      -- Highlight and trim trailing whitespaces.
      require('mini.trailspace').setup {}
      local command = vim.api.nvim_create_user_command
      command('TrimWhitespaces', require('mini.trailspace').trim, { bang = true })
   end
} --}}}

-- use 'airblade/vim-rooter'
use { 'ahmedkhalf/project.nvim', -- {{{
   config = function()
      require('project_nvim').setup {
         patterns = {
            '.git', '_darcs', '.hg', '.bzr', '.svn', 'Makefile', 'package.json',
            '>.config', '>roles'
         },
         -- When set to false, you will get a message
         -- when project.nvim changes your directory.
         silent_chdir = true,
      }
   end
} -- }}}

-- use { 'klen/nvim-config-local', as = 'per-project-config', -- {{{
--    config = function()
--       require('config-local').setup {
--          -- Config file patterns to load (lua supported)
--          config_files = { '.vimrc.lua', '.vimrc' },
--          autocommands_create = true, -- Create autocommands (VimEnter, DirectoryChanged)
--          commands_create = false, -- Create commands (ConfigSource, ConfigEdit, ConfigTrust, ConfigIgnore)
--          silent = false, -- Disable plugin messages (Config loaded/ignored)
--          lookup_parents = true, -- Lookup config files in parent directories
--       }
--    end
-- } --}}}

-- -- Make 'gf' keybinding work in different filetypes.  For instructions of what
-- -- it can do more see: https://github.com/tpope/vim-apathy and ":help include-search"
-- use 'tpope/vim-apathy'

-- Asynchronous build and test dispatcher
use { 'tpope/vim-dispatch', -- {{{
   requires = 'radenling/vim-dispatch-neovim',
   cmd = { 'Dispatch', 'Make', 'Focus', 'Start' }
} -- }}}

use { 'stevearc/dressing.nvim', --{{{
   requires = 'MunifTanjim/nui.nvim',
   config = function() require('anuvyklack/dressing') end
} --}}}

use 'wellle/targets.vim' -- Additional text objects
use 'tpope/vim-unimpaired' -- Bidirectional motions: switch buffers, add blank lines, etc.
use { 'tpope/vim-surround', as = 'surround', requires = 'tpope/vim-repeat' }

-- Makes *-operator works the way it should by default
use { 'haya14busa/vim-asterisk', as = 'asterisk', --{{{
   config = function()
      vim.g['asterisk#keeppos'] = 0 -- Keep cursor position inside word between jumps.
      require('keymaps').asterisks()
   end
} --}}}

-- use 'AndrewRadev/splitjoin.vim'
use { 'AckslD/nvim-trevJ.lua', -- {{{
   module = 'trevj', -- lazy load
   config = function()
      require('trevj').setup()
   end
} -- }}}

use { 'junegunn/vim-easy-align', as = 'easy-align',
   config = function() require('keymaps').easy_align() end }

-- Marks
-- use 'kshenoy/vim-signature'  -- display and navigate marks
use { 'chentoast/marks.nvim', --{{{
   config = function()
      require 'marks'.setup {
         -- https://github.com/chentau/marks.nvim/issues/40
         -- 0 or some really large value to disable mark tracking
         refresh_interval = 0,

         default_mappings = true,
         -- builtin_marks = { ".", "<", ">", "^", "'" }, -- which builtin marks to show
         signs = true,
         excluded_filetypes = { 'gitcommit' },
         mappings = {}
      }
   end
} --}}}

use { 'kevinhwang91/nvim-bqf', as = 'better-quickfix', --{{{
   ft = 'qf',
   config = function()
      require('bqf').setup {
         auto_enable = true,
         auto_resize_height = true, -- highly recommended enable
         preview = {
            buf_label = false,
         }
      }
   end
} --}}}

-- use 'matze/vim-move'
use { 'booperlv/nvim-gomove', as = 'move', --{{{
   config = function()
      require("gomove").setup({
         -- Enable to move past last column when moving blocks horizontally.
         move_past_end_col = true,
      })
   end
} --}}}

-- use 'simnalamburt/vim-mundo'  -- another undo tree visualizer
use { 'mbbill/undotree', --{{{
   config = function()
      local g = vim.g
      g.undotree_HighlightChangedWithSign = 0
      g.undotree_WindowLayout = 2
   end
} --}}}

-- use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }

-- -- https://github.com/junegunn/vim-peekaboo/issues/74
-- use { 'junegunn/vim-peekaboo',
--    config = function()
--       vim.g.peekaboo_window = 'vertical botright 40new'
--    end
-- }

use { 'jbyuki/venn.nvim', -- Draw ASCII diagrams {{{
   config = function()
      require('keymaps').draw_diagrams()
   end
} -- }}}

use { 'frabjous/knap', --{{{
   ft = { 'markdown', 'tex' },
   config = function()
      vim.g.knap_settings = {
         mdtohtml = "pandoc --standalone %docroot% -o %outputfile% --css ~/.config/nvim/assets/github-pandoc.css",
      }
      require('keymaps').knap()
   end
} --}}}

-- Execute :StartupTime to get an averaged startup profile.
use { 'tweekmonster/startuptime.vim', as = 'startuptime',
   cmd = 'StartupTime' }

-- Lua plugins development {{{
use { 'rafcamlet/nvim-luapad', as = 'luapad',
   config = function()
      local command = vim.api.nvim_create_user_command
      local luapad = require('luapad')

      -- command('Luapad', function()
      --    vim.cmd 'vnew'
      --    require('luapad.evaluator'):new { buf = 0 }:start()
      -- end, {})
      command('LuaAttach', luapad.attach, { bang = true })
      command('LuaDetach', luapad.detach, { bang = true })
      command('LuaToggle', luapad.toggle, { bang = true })

      vim.api.nvim_create_autocmd('BufEnter', {
         pattern = '*_Luapad.lua',
         command = 'setlocal bufhidden=hide'
      })
   end
}
--}}}

-- use { 'sidebar-nvim/sidebar.nvim',
--       config = function() require('anuvyklack/sidebar') end }

-- -- Litee {{{
-- use { 'ldelossa/litee.nvim',
--    config = function() require('litee.lib').setup({
--       tree = {
--          icon_set = "codicons"
--       },
--       -- panel = {
--       --    orientation = 'right',
--       --    panel_size  = 30
--       -- }
--    }) end
-- }
--
-- use { 'ldelossa/litee-calltree.nvim',
--    config = function() require('litee.calltree').setup() end
-- }
--
-- -- use { 'ldelossa/litee-symboltree.nvim',
-- --    config = function() require('litee.symboltree').setup({
-- --       icon_set = "codicons",
-- --       -- on_open = 'panel', -- "panel" or "popout"
-- --    }) end
-- -- }
--
-- -- use { 'ldelossa/litee-bookmarks.nvim',
-- --    config = function() require('litee.bookmarks').setup({}) end
-- -- }
--
-- -- }}}

-- use { 'vim-pandoc/vim-pandoc', --{{{
--    requires = {'vim-pandoc/vim-pandoc-syntax', opt = true},
--    ft = {'markdown', 'pandoc'},
--    config = function()
--       vim.cmd("source ~/.config/nvim/lua/anuvyklack/pandoc.vim")
--    end
-- } --}}}

-- use { 'kevinhwang90/nvim-hlslens', as = 'hlslens',
--       config = function() require('anuvyklack/hlslens') end }

-- <leader>? : 40-column cheat sheet
-- use { 'anuvyklack/vim-cheat40', as = 'cheat40' } -- my fork
-- use { 'sudormrfbin/cheatsheet.nvim', --{{{
--    requires = {
--       -- { 'nvim-telescope/telescope.nvim', as = 'telescope' },
--       'nvim-lua/popup.nvim',
--       'nvim-lua/plenary.nvim',
--    },
--    config = function() require("cheatsheet").setup{} end
-- } --}}}

-- use { 'anuvyklack/help-vsplit.nvim', as = 'help-vsplit', --{{{
-- -- use { '~/Git/my_neovim-plugins/help-vsplit.nvim', as = 'help-vsplit',
--    config = function() require('help-vsplit').setup {
--       always = true,
--       side = 'left'
--    } end
-- } --}}}

-- Syntax ------------------------------------------------------------------ {{{

-- Show syntax highlighting attributes of character under cursor.
use 'vim-scripts/SyntaxAttr.vim'

-- Automatically adjusts 'shiftwidth' and 'expandtab' heuristically based on
-- the current file.
use { 'tpope/vim-sleuth', --{{{
   cmd = { 'Sleuth', 'verbose Sleuth' },
   config = function()
      -- Turn off automatic detection, потому что думает за меня и думает лишнего.
      vim.g.sleuth_automatic = 0
   end
} --}}}

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

use { 'mzlogin/vim-markdown-toc', ft = 'markdown' }

--}}}

-- Orgmode ----------------------------------------------------------------- {{{

-- use { 'kristijanhusak/orgmode.nvim',
--    config = function()
--       require('orgmode').setup{
--          org_hide_emphasis_markers = true,
--       }
--    end
-- }

use { 'nvim-neorg/neorg',
   requires = 'nvim-lua/plenary.nvim',
   after = 'treesitter',
   ft = 'norg',
   cmd = 'Neorg',
   run = ':Neorg sync-parsers',
   config = function() require('anuvyklack/neorg') end
}

--}}}

-- Русский язык (Switch language) ------------------------------------------ {{{
use { 'lyokha/vim-xkbswitch', as = 'xkbswitch',
   config = function()
      vim.g.XkbSwitchEnabled = 1
      vim.g.XkbSwitchLib = '/usr/local/lib/libg3kbswitch.so'
      vim.g.XkbSwitchAssistNKeymap = 1 -- for commands r and f
      -- vim.g.XkbSwitchIMappings = {'ru'}

      -- require('which-key').register {
      --    gh = 'which_key_ignore',
      --    gH = 'test description',
      --    ['g<C-H>'] = 'which_key_ignore'
      -- }
   end
}
-- use 'powerman/vim-plugin-ruscmd'

--}}}

-- Tmux integration -------------------------------------------------------- {{{
use 'tmux-plugins/vim-tmux' -- tmux.conf syntax

use { 'anuvyklack/vim-tmux-navigator', -- my fork
   config = function()
      -- Activate autoupdate on exit.
      vim.g.tmux_navigator_save_on_switch = 0

      -- Disable vim->tmux navigation when the Vim pane is zoomed in tmux.
      vim.g.tmux_navigator_disable_when_zoomed = 1
   end
}
---------------------------------------------------------------------------- }}}

return setmetatable({}, { __index = function(_, key) return packer[key] end })

-- vim: fdm=marker fml=1
