local g = vim.g

-- Display indent markers when folders are open.
g.nvim_tree_indent_markers = 1

-- Highlight changed git files.
g.nvim_tree_git_hl = 0

-- See :help filename-modifiers for more options.
g.nvim_tree_root_folder_modifier = ':~'

-- Append a trailing slash to folder names.
g.nvim_tree_add_trailing = 0

-- Compact folders that only contain a single folder into one node in the file tree.
g.nvim_tree_group_empty = 1

-- List of filenames that gets highlighted with NvimTreeSpecialFile
g.nvim_tree_special_files = {
    'README', 'README.md', 'Makefile', 'MAKEFILE', 'CMakeLists.txt'
}

-- Used as a separator between symlinks' source and target.
g.nvim_tree_symlink_arrow = ' -> '

-- Will change cwd of nvim-tree to that of new buffer's when opening nvim-tree.
g.nvim_tree_respect_buf_cwd = 1

g.nvim_tree_show_icons = {
   git = 0,
   folders = 1,
   files = 1,
   folder_arrows = 1,
}

--   diff
--   diff added
--   diff ignored
--   diff modified
--   diff removed
--   diff renamed

--    
-- 
--  
--  
-- 

g.nvim_tree_icons = {
   default = '',
   symlink = '',
   git = {
      unstaged  = "•", --   ✗  
      staged    = "", -- ✓   ﯂  
      unmerged  = "",
      renamed   = "", --  
      untracked = "★",
      deleted   = "",
      ignored   = "" --   ◌ 
   },
   folder = {
      arrow_open   = "", --   
      arrow_closed = "", --   
      default      = "",
      open         = "",
      empty        = "",
      empty_open   = "",
      symlink      = "",
      symlink_open = "",
   },
   lsp = {
      hint    = "",
      info    = "",
      warning = "",
      error   = "",
   }
}


local tree_cb = require('nvim-tree.config').nvim_tree_callback

require('nvim-tree').setup {
   disable_netrw = true,  -- Disables netrw completely.
   hijack_netrw = true,   -- Hijack netrw window on startup.
   open_on_setup = false, -- Open the tree when running this setup function.

   -- Will not open on setup if the filetype is in this list.
   ignore_ft_on_setup = {},

   -- Closes neovim automatically when the tree is the last **WINDOW** in the view.
   auto_close = true,

   -- Opens the tree when changing/opening a new tab if the tree wasn't
   -- previously opened.
   open_on_tab = false,

   -- Hijack the cursor in the tree to put it at the start of the filename.
   hijack_cursor = true,

   diagnostics = {  -- Show lsp diagnostics in the signcolumn.
      enable = false,
      icons = { hint = "", info = "", warning = "", error = "" }
   },

   -- Updates the root directory of the tree on `DirChanged` (when your run
   -- `:cd` usually).
   update_cwd = true,

   -- Update the focused file on `BufEnter`, un-collapses the folders
   -- recursively until it finds the file.
   update_focused_file = {
      enable = true,  -- Enables the feature.
      -- Update the root directory of the tree to the one of the folder
      -- containing the file if the file is not under the current root
      -- directory.
      -- Only relevant when `update_focused_file.enable` is true.
      update_cwd = true,
      -- List of buffer names / filetypes that will not update the cwd if the
      -- file isn't found under the current root directory.
      -- Only relevant when `update_focused_file.update_cwd` is true and
      -- `update_focused_file.enable` is true.
      ignore_list = {}
   },

   -- Configuration options for the system open command (`s` in the tree by
   -- default).
   system_open = {
      -- The command to run this, leaving nil should work in most cases.
      cmd = nil,
      -- The command arguments as a list.
      args = {}
   },

   git = {  -- git integration with icons and colors
      enable = true,
      ignore = true, -- ignore files based on `.gitignore`
   },

   view = {
      -- Width of the window, can be either a number (columns) or a string in `%`.
      width = 35,  -- 30 by default
      -- Side of the tree, can be one of 'left' | 'right' | 'top' | 'bottom'
      side = 'left',
      -- If true the tree will resize itself after opening a file.
      auto_resize = true, -- false by default
      mappings = {
         -- Custom only false will merge the list with the default mappings.
         -- If true, it will only use your list to set the mappings.
         custom_only = false,
         -- List of mappings to set on the tree manually.
         list = {
            { key = '?', cb = tree_cb("toggle_help") }  -- help UI
         }
      }
   },

   filters = {
      dotfiles = true,  -- Hides files and folders starting with a dot.
      custom = { '.git', 'tags', '.netrwhist' }
   },
}

require('keybindings').nvim_tree()
