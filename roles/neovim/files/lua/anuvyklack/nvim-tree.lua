require('nvim-tree').setup {
   hijack_cursor = false,
   hijack_netrw = true,
   hijack_unnamed_buffer_when_opening = true,
   sync_root_with_cwd = true,
   reload_on_bufenter = false,
   respect_buf_cwd = true,
   view = {
      adaptive_size = false,
      centralize_selection = true,
      width = 40,
      height = 40,
      hide_root_folder = false,
      signcolumn = "yes",
      float = {
         enable = false,
         open_win_config = {
            width = 40,
            height = 40,
         }
      },
   },
   renderer = {
      add_trailing = false,
      group_empty = true,
      highlight_git = false,
      full_name = true,
      highlight_opened_files = "icon",
      icons = {
         git_placement = "after",
         symlink_arrow = " -> ",
         show = {
           git = true,
         },
         glyphs = {
            default = "",
            symlink = "",
            bookmark = "",
            folder = {
               arrow_closed = "",
               arrow_open = "",
               default = "",
               open = "",
               empty = "",
               empty_open = "",
               symlink = "",
               symlink_open = "",
            },
            git = {
               unstaged = "✗",
               staged = "✓",
               unmerged = "",
               renamed = "➜",
               untracked = "★",
               deleted = "",
               ignored = "◌",
            },
         },
      },
      special_files = { "Makefile", "README.md", "readme.md", "Cargo.toml" },
   },
   hijack_directories = {
      enable = false, -- disable if use vim-dirvish or dirbuf.nvim
      auto_open = true,
   },
   update_focused_file = {
      enable = true,
      update_root = true,
      ignore_list = {},
   },
   root_dirs = {},
   prefer_startup_root = true,
   ignore_ft_on_setup = {}, -- if use vim-startify
   diagnostics = {
      enable = true,
      show_on_dirs = true,
      debounce_delay = 50,
   },
   filters = {
      dotfiles = false,
      custom = {},
      exclude = {},
   },
   git = {
      enable = true,
      ignore = true,
      show_on_dirs = true,
      timeout = 400,
   },
   actions = {
      change_dir = {
         enable = true,
      },
      file_popup = {
         open_win_config = {
            col = 1,
            row = 1,
            relative = "cursor",
            border = "shadow",
            style = "minimal",
         },
      },
      open_file = {
         quit_on_open = false,
         resize_window = true,
         window_picker = {
            enable = true,
            chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
            exclude = {
               filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
               buftype = { "nofile", "terminal", "help" },
            },
         },
      },
   },
   live_filter = {
      prefix = "[FILTER]: ",
      always_show_folders = true,
   }
}

-- require('keymaps').nvim_tree()

require('keymaps').file_tree()

-- vim.cmd [[
-- autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif
-- ]]
