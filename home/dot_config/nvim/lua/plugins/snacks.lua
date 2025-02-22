return {
   {
      "folke/snacks.nvim",
      opts = {
         indent = { -- Indent guides
            enabled = true,
            animate = { enabled = false },
         },
         scroll = { enabled = false },
         picker = {
            layout = {
               cycle = true,
               preset = function()
                  --- Use the default layout or vertical if the window is too narrow
                  return vim.o.columns >= 200 and "default" or "vertical"
               end,
            },
            formatters = {
               file = {
                  truncate = 80 -- truncate the file path to (roughly) this length
               }
            },
            layouts = {
               vertical = {
                  layout = { width = 0.75 }
               }
            }
         },
      },
   },
}

