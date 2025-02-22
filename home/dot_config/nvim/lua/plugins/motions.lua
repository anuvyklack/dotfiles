local keymap = vim.keymap

---@param command string
---@return string `<Cmd>..command..<CR>`
local function cmd(command)
   return table.concat({ "<Cmd>", command, "<CR>" })
end

return {
   {
      "folke/flash.nvim",
      opts = {
         jump = { autojump = true },
         search = { multi_window = false },
      },
   },
   {
      "smoka7/hop.nvim", version = "*",
      opts = function()
         return {
            distance_method = require('hop.hint').readwise_distance
         }
      end,
      keys= {
         { ";w", cmd("HopWordAC"), mode = { "n", "x" }, desc = "Easymotion forward word" },
         { ";b", cmd("HopWordBC"), mode = { "n", "x" }, desc = "Easymotion bacward word" },
         { ";j", cmd("HopLineAC"), mode = { "n", "x" }, desc = "Easymotion line up" },
         { ";k", cmd("HopLineBC"), mode = { "n", "x" }, desc = "Easymotion line down" },
      },
   },
   {
      "chrisgrieser/nvim-spider", lazy = true,
      keys = {
         -- For dot-repeat to work, you have to call the motions as Ex-commands.
         -- Dot-repeat will not work if write:
         -- function() require("spider").motion("w")
         -- end as third argument.
         { "w", cmd "lua require('spider').motion('w')", mode = { "n", "o", "x" } },
         { "e", cmd "lua require('spider').motion('e')", mode = { "n", "o", "x" } },
         { "b", cmd "lua require('spider').motion('b')", mode = { "n", "o", "x" } },
      },
   },
}
