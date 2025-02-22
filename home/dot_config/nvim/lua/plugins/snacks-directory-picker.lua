local function get_directories()
   local directories = {}

   local handle = io.popen("fd . --type directory")
   if handle then
      for line in handle:lines() do
         table.insert(directories, line)
      end
      handle:close()
   else
      print("Failed to execute fd command")
   end

   return directories
end


vim.keymap.set("n", "<leader>fd", function()
   local Snacks = require("snacks")
   local dirs = get_directories()

   ---@type snacks.picker.Config
   return Snacks.picker({
      finder = function()
         local items = {}
         for i, item in ipairs(dirs) do
            table.insert(items, {
               idx = i,
               file = item,
               text = item,
            })
         end
         return items
      end,
      -- layout = {
      --    layout = {
      --       box = "horizontal",
      --       width = 0.5,
      --       height = 0.5,
      --       {
      --          box = "vertical",
      --          border = "rounded",
      --          title = "Find directory",
      --          { win = "input", height = 1, border = "bottom" },
      --          { win = "list", border = "none" },
      --       },
      --    },
      -- },
      format = function(item, _)
         local file = item.file
         local ret = {}
         local align = Snacks.picker.util.align
         local icon, icon_hl = Snacks.util.icon(file.ft, "directory")
         ret[#ret + 1] = { align(icon, 3), icon_hl }
         ret[#ret + 1] = { " " }
         ret[#ret + 1] = { align(file, 20) }

         return ret
      end,
      -- confirm = function(picker, item)
      --    picker:close()
      --    Snacks.picker.pick("files", {
      --       dirs = { item.file },
      --    })
      -- end,
   })
end)

return {}
