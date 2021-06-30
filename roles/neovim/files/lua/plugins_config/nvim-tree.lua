local g = vim.g


g.nvim_tree_width = 35  -- 30 by default

-- Display indent markers when folders are open.
g.nvim_tree_indent_markers = 1

g.nvim_tree_ignore = { '.git', 'tags', '.netrwhist' }

-- List of filenames that gets highlighted with NvimTreeSpecialFile
g.nvim_tree_special_files = {
    'README', 'README.md', 'Makefile', 'MAKEFILE', 'CMakeLists.txt'
}

g.nvim_tree_auto_open = 0    -- Opens the tree when typing `vim $DIR` or `vim`.
g.nvim_tree_auto_close = 1   -- Closes the tree when it's the last window.
g.nvim_tree_quit_on_open = 0 -- Closes the tree when you open a file.

-- Allows the cursor to be updated when entering a buffer.
g.nvim_tree_follow = 0

-- Hides files and folders starting with a dot.
g.nvim_tree_hide_dotfiles = 1

-- Highlight changed git files and so on.
g.nvim_tree_git_hl = 0

-- WIll open the tree when entering a new tab
-- and the tree was previously open.
g.nvim_tree_tab_open = 1

g.nvim_tree_width_allow_resize  = 1

-- Append a trailing slash to folder names.
g.nvim_tree_add_trailing = 0

-- Compact folders that only contain a single
-- folder into one node in the file tree.
g.nvim_tree_group_empty = 1

-- Show lsp diagnostics in the signcolumn.
-- See :help nvim_tree_lsp_diagnostics
g.nvim_tree_lsp_diagnostics = 1

g.nvim_tree_show_icons = {
    git = 0,
    folders = 1,
    files = 1,
}

require('keybindings').nvim_tree()
