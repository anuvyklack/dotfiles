require('neorg').setup({
   load = { -- what modules to load
      ["core.defaults"] = {}, -- Load all default modules.
      ["core.norg.esupports.indent"] = { config = {
         -- tweaks = {
         --    heading2 = 1,
         --    heading3 = 2,
         --    heading4 = 3,
         --    heading5 = 4,
         --    heading6 = 5,
         -- }
      }},
      ["core.keybinds"] = { config = {
         default_keybinds = true, -- Generate the default keybinds
         -- neorg_leader = '<LocalLeader>' -- This is the default if unspecified
      }},
      ["core.norg.concealer"] = { config = {
         markup_preset = 'dimmed',
         icons = {
            todo = {
               enabled = true,
               done = {
                  -- icon = "", -- default: 
                  -- icon = "", -- e876: (done) Material Icons
                  icon = "󰸞", -- f0e1e: 󰸞  (mdi-check-bold) Material Design Icons
               },
               pending = {
                  -- icon = "•",
                  icon = "󱎫", -- U+f13ab: Material Design Icons
                  -- icon = "", -- U+e425: Material Icons
                  -- icon = "",
                  -- -- Do not ofset the first '[' bracker.
                  -- extract = function() return 0 end,
                  -- icon = "  ",
               },
               undone = {
                  icon = " ", -- default: ×
               },
               uncertain = {
                  icon = "",
               },
               on_hold = {
                  -- enabled = true,
                  icon = "",
                  -- highlight = "NeorgTodoItemOnHoldMark",
                  -- query = "(todo_item_on_hold) @icon",
                  -- extract = function() return 1 end,
               },
               cancelled = {
                  icon = "",
                  -- icon = "",
               },
               recurring = {
                  -- icon = "",
                  -- icon = "",
                  -- icon = "",
                  icon = "󰑐",
               },
               urgent = {
                  icon = "",
               },
            },
            list = {
               enabled = false,
               level_1 = {
                  enabled = true,
                  icon = "•",
                  highlight = "NeorgUnorderedList1",
                  query = "(unordered_list1_prefix) @icon",
               },
               level_2 = {
                  enabled = true,
                  -- icon = " •",
                  icon = "••", -- •
                  highlight = "NeorgUnorderedList2",
                  query = "(unordered_list2_prefix) @icon",
                  -- extract = function() return 1 end,
               },
               level_3 = {
                  enabled = true,
                  icon = "--•",
                  highlight = "NeorgUnorderedList3",
                  query = "(unordered_list3_prefix) @icon",
               },
               -- level_4 = {
               --    enabled = true,
               --    icon = "   •",
               --    highlight = "NeorgUnorderedList4",
               --    query = "(unordered_list4_prefix) @icon",
               -- },
               -- level_5 = {
               --    enabled = true,
               --    icon = "•",
               --    highlight = "NeorgUnorderedList5",
               --    query = "(unordered_list5_prefix) @icon",
               -- },
               -- level_6 = {
               --    enabled = true,
               --    icon = " •",
               --    highlight = "NeorgUnorderedList6",
               --    query = "(unordered_list6_prefix) @icon",
               -- },
            },
            definition = { enabled = false },
            delimiter = { enabled = false },
         }
      }},
      ["core.norg.dirman"] = { config = { -- Manage your directories with Neorg.
         workspaces = {
           -- Format: <name_of_workspace> = <path_to_workspace_root>
            notes = "~/notes",
            example_gtd = "~/notes/example_workspaces/gtd",
         },
         index = "index.norg", -- The name of the main (root) .norg file.
         -- Automatically change the directory to the current workspace's root
         -- every time.
         autochdir = true,

         -- The location to write and read the workspace cache file.
         last_workspace = vim.fn.stdpath("cache").."/neorg_last_workspace.txt"
      }},
      -- ["core.gtd.base"] = { config = {
      --    workspace = "example_gtd",
      -- },},
      ["core.norg.completion"] = { config = {
         engine = "nvim-cmp"
      }}
   },
})

-- vim: fdc=1 fml=1
