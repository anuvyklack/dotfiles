local g = vim.g

g.nvim_tree_width = 35  -- 30 by default

g.nvim_tree_ignore = { '.git', 'tags', '.netrwhist' }

g.nvim_tree_gitignore = 1  -- 0 by default

g.nvim_tree_auto_open = 0    -- Opens the tree when typing `vim $DIR` or `vim`.
g.nvim_tree_auto_close = 1   -- Closes the tree when it's the last window.
g.nvim_tree_quit_on_open = 0 -- Closes the tree when you open a file.

-- Don't auto open tree on specific filetypes.
g.nvim_tree_auto_ignore_ft = { 'startify', 'dashboard' }

-- Update the cursor position when entering a buffer.
g.nvim_tree_follow = 0

-- Display indent markers when folders are open.
g.nvim_tree_indent_markers = 1

-- Hides files and folders starting with a dot.
g.nvim_tree_hide_dotfiles = 1

-- Highlight changed git files.
g.nvim_tree_git_hl = 0

-- See :help filename-modifiers for more options.
g.nvim_tree_root_folder_modifier = ':~'

-- WIll open the tree when entering a new tab
-- and the tree was previously open.
g.nvim_tree_tab_open = 1

-- Resize the tree to its saved width when opening a file.
g.nvim_tree_auto_resize = 1 -- 1 by default

g.nvim_tree_disable_netrw = 0

-- Append a trailing slash to folder names.
g.nvim_tree_add_trailing = 0

-- Compact folders that only contain a single
-- folder into one node in the file tree.
g.nvim_tree_group_empty = 1

-- Show lsp diagnostics in the signcolumn.
-- See :help nvim_tree_lsp_diagnostics
g.nvim_tree_lsp_diagnostics = 1

g.nvim_tree_window_picker_exclude = {
  filetype = { 'packer', 'qf' },
  buftype = { 'terminal' },
}

-- List of filenames that gets highlighted with NvimTreeSpecialFile
g.nvim_tree_special_files = {
    'README', 'README.md', 'Makefile', 'MAKEFILE', 'CMakeLists.txt'
}

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

require('keybindings').nvim_tree()

-- vim: nofen
