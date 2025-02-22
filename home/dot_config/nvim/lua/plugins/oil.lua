return {
   "stevearc/oil.nvim",
   -- Lazy loading is not recommended because it is very tricky to make it work
   -- correctly in all situations.
   lazy = false,
   keys = {
      -- stylua: ignore
      { "-", function() require("oil").open() end, desc = "Oil" },
      {
         "gd",
         function()
            _G.detail_oil_view = not _G.detail_oil_view
            if _G.detail_oil_view then
               require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
            else
               require("oil").set_columns({ "icon" })
            end
         end,
         desc = "Toggle file detail view",
      },
   },
   ---@module 'oil'
   ---@type oil.SetupOpts
   opts = {
      -- default_file_explorer = true,
      -- view_options = {
      --    show_hidden = true,
      -- },
      skip_confirm_for_simple_edits = true,
      watch_for_changes = true,
   },
}
