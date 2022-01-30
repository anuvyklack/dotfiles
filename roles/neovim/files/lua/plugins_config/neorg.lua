require('neorg').setup {
   load = { -- what modules to load
      ["core.defaults"] = {}, -- Load all the default modules.
      ["core.keybinds"] = { config = {
         default_keybinds = true, -- Generate the default keybinds
         neorg_leader = "<Leader>o" -- This is the default if unspecified
      } },
      ["core.norg.concealer"] = { config = {
         icons = {
            todo = {
               enabled = true,
               -- done = {
               --    enabled = true,
               --    icon = "",
               --    highlight = "NeorgTodoItemDoneMark",
               --    query = "(todo_item_done) @icon",
               --    extract = function() return 1 end,
               -- },
               pending = {
                  enabled = true,
                  icon = "",
                  -- icon = "ﮫ",
                  -- icon = "",
                  highlight = "NeorgTodoItemPendingMark",
                  query = "(todo_item_pending) @icon",
                  extract = function() return 1 end,
               },
               undone = {
                  enabled = true,
                  icon = " ", -- default: ×
                  highlight = "NeorgTodoItemUndoneMark",
                  query = "(todo_item_undone) @icon",
                  extract = function() return 1 end,
               },
               -- uncertain = {
               --    enabled = true,
               --    icon = "",
               --    highlight = "NeorgTodoItemUncertainMark",
               --    query = "(todo_item_uncertain) @icon",
               --    extract = function() return 1 end,
               -- },
               -- on_hold = {
               --    enabled = true,
               --    icon = "",
               --    highlight = "NeorgTodoItemOnHoldMark",
               --    query = "(todo_item_on_hold) @icon",
               --    extract = function() return 1 end,
               -- },
               -- cancelled = {
               --    enabled = true,
               --    icon = "",
               --    highlight = "NeorgTodoItemCancelledMark",
               --    query = "(todo_item_cancelled) @icon",
               --    extract = function() return 1 end,
               -- },
               -- recurring = {
               --    enabled = true,
               --    icon = "⟳",
               --    highlight = "NeorgTodoItemRecurringMark",
               --    query = "(todo_item_recurring) @icon",
               --    extract = function() return 1 end,
               -- },
               -- urgent = {
               --    enabled = true,
               --    icon = "⚠",
               --    highlight = "NeorgTodoItemUrgentMark",
               --    query = "(todo_item_urgent) @icon",
               --    extract = function() return 1 end,
               -- },
            },

            list = {
               enabled = true,
               level_1 = {
                  enabled = true,
                  icon = "•",
                  highlight = "NeorgUnorderedList1",
                  query = "(unordered_list1_prefix) @icon",
               },
               level_2 = {
                  enabled = true,
                  icon = " •",
                  highlight = "NeorgUnorderedList2",
                  query = "(unordered_list2_prefix) @icon",
               },
               -- level_3 = {
               --    enabled = true,
               --    icon = "  •",
               --    highlight = "NeorgUnorderedList3",
               --    query = "(unordered_list3_prefix) @icon",
               -- },
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

            link = {
               enabled = true,
               -- level_1 = {
               --    enabled = true,
               --    icon = " ",
               --    highlight = "NeorgUnorderedLink1",
               --    query = "(unordered_link1_prefix) @icon",
               -- },
               -- level_2 = {
               --    enabled = true,
               --    icon = "  ",
               --    highlight = "NeorgUnorderedLink2",
               --    query = "(unordered_link2_prefix) @icon",
               -- },
               -- level_3 = {
               --    enabled = true,
               --    icon = "   ",
               --    highlight = "NeorgUnorderedLink3",
               --    query = "(unordered_link3_prefix) @icon",
               -- },
               -- level_4 = {
               --    enabled = true,
               --    icon = "",
               --    highlight = "NeorgUnorderedLink4",
               --    query = "(unordered_link4_prefix) @icon",
               -- },
               -- level_5 = {
               --    enabled = true,
               --    icon = " ",
               --    highlight = "NeorgUnorderedLink5",
               --    query = "(unordered_link5_prefix) @icon",
               -- },
               -- level_6 = {
               --    enabled = true,
               --    icon = "  ",
               --    highlight = "NeorgUnorderedLink6",
               --    query = "(unordered_link6_prefix) @icon",
               -- },
            },
         },
         markup_preset = 'dimmed',
      } },
      ["core.norg.dirman"] = { config = { -- Manage your directories with Neorg.
         workspaces = {
            my_workspace = "~/neorg"
         }
      } },
      ["core.norg.completion"] = { config = {
         engine = "nvim-cmp"
      } }
   },
}

-- vim: fdc=1
