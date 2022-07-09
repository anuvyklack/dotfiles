require('fidget').setup {
   text = {
      spinner = 'dots',
      -- character shown when all tasks are complete
      done = "󰸞", -- f0e1e: 󰸞  (mdi-check-bold)
      commenced = "Started",    -- message shown when task starts
      completed = "Completed",  -- message shown when task completes
   },
   window = {
      blend = 0,  -- &winblend for the window
   },
   fmt = {
      stack_upwards = true,  -- list of tasks grows upwards
   }
}
