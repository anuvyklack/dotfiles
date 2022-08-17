vim.g.neo_tree_remove_legacy_commands = 1

require('neo-tree').setup({
   close_if_last_window = true, -- Close Neo-tree if it is the last window in the tab.
   popup_border_style = 'rounded',
   enable_git_status = true,
   enable_diagnostics = false,
   default_component_configs = {
      icon = {
         folder_closed = "", --    
         folder_open = "", -- 
         folder_empty = "", -- 
         default = "", -- * 
      },
      name = {
         trailing_slash = false,
         use_git_status_colors = false,
         highlight = "NeoTreeFileName",
      },
      git_status = {
         symbols = {
            -- Change type
            added     = "+", -- ✚
            deleted   = "x", -- ✖
            modified  = "", -- 
            renamed   = "", -- 
            -- Status type
            untracked = "?", -- 
            ignored   = "", -- 
            unstaged  = "", -- 
            staged    = "", -- 
            conflict  = "", -- 
         }
      },
   },
   window = {
      width = 40,
      mappings = {
         ["<space>"] = 'none', -- "toggle_node",
         ["<2-LeftMouse>"] = "open",
         ["<CR>"] = "open",
         ["S"] = "open_split",
         ["s"] = "open_vsplit",
         ["t"] = "open_tabnew",
         ["w"] = "open_with_window_picker",
         ["C"] = "close_node",
         ["<BS>"] = "navigate_up",
         ["."] = "set_root",
         ["zh"] = "toggle_hidden",
         ["/"] = "fuzzy_finder",
         ["f"] = "filter_on_submit",
         ["<C-x>"] = "clear_filter",
         ["a"] = "add",
         ["A"] = "add_directory",
         ["d"] = "delete",
         ["r"] = "rename",
         ["y"] = "copy_to_clipboard",
         ["x"] = "cut_to_clipboard",
         ["p"] = "paste_from_clipboard",
         ["c"] = "copy", -- takes text input for destination
         ["m"] = "move", -- takes text input for destination
         ["q"] = "close_window",
         ["R"] = "refresh",
         ["?"] = "show_help",
      }
   },
   -- nesting_rules = {},
   filesystem = {
      filtered_items = {
         visible = false, -- when true, they will just be displayed differently than normal items
         hide_dotfiles = true,
         hide_gitignored = true,
         hide_by_name = {
            -- ".DS_Store",
            --"node_modules"
         },
         never_show = { -- remains hidden even if visible is toggled to true
            "thumbs.db"
         },
      },
      -- This will find and focus the file in the active buffer every time the
      -- current file is changed while the tree is open.
      follow_current_file = true,

      -- "open_default" - netrw disabled, opening a directory opens neo-tree
      --                  in whatever position is specified in window.position
      -- "open_current" - netrw disabled, opening a directory opens within the
      --                  window like netrw would, regardless of window.position
      -- "disabled"     - netrw left alone, neo-tree does not handle opening dirs
      hijack_netrw_behavior = "disabled",

      -- This will use the OS level file watchers to detect changes instead of
      -- relying on nvim autocmd events.
      use_libuv_file_watcher = true,
   },
   -- buffers = {
   --    show_unloaded = true,
   --    window = {
   --       mappings = {
   --          ["bd"] = "buffer_delete",
   --       }
   --    },
   -- },
   -- git_status = {
   --    window = {
   --       position = "float",
   --       mappings = {
   --          ["A"]  = "git_add_all",
   --          ["gu"] = "git_unstage_file",
   --          ["ga"] = "git_add_file",
   --          ["gr"] = "git_revert_file",
   --          ["gc"] = "git_commit",
   --          ["gp"] = "git_push",
   --          ["gg"] = "git_commit_and_push",
   --       }
   --    }
   -- }
})

require('keymaps').file_tree()
