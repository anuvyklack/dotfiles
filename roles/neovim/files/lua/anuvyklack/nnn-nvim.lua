local available, nnn = pcall(require, 'nnn')
if not available then return end

nnn.setup{
   explorer = {
      cmd = "nnn -CD",   -- command overrride (-F1 flag is implied, -a flag is invalid!)
      width = 30,       -- width of the vertical split
      side = "topleft", -- or "botright", location of the explorer window
      session = "",     -- or "global" / "local" / "shared"
      tabs = true,      -- seperate nnn instance per tab
   },
   picker = {
      cmd = "nnn -CD",   -- command override (-p flag is implied)
      -- cmd = "tmux new-session nnn -Pw",
      style = {
         width = 0.6,     -- width in percentage of the viewport
         height = 0.8,    -- height in percentage of the viewport
         xoffset = 0.5,   -- xoffset in percentage
         yoffset = 0.5,   -- yoffset in percentage
         border = "rounded" --   Border decoration for example "rounded"
                            -- (:h nvim_open_win).
      },
      session = "",  -- or "global" / "local" / "shared"
   },
   auto_open = {
      setup = nil,    -- or "explorer" / "picker", auto open on setup function
      tabpage = nil,  -- or "explorer" / "picker", auto open when opening new tabpage
      empty = false,  -- only auto open on empty buffer
      ft_ignore = {   -- dont auto open for these filetypes
         "gitcommit",
      }
   },
   auto_close = true,   -- close tabpage/nvim when nnn is last window
   replace_netrw = 'pickers', -- or "explorer" / "picker"
   mappings = {},       -- table containing mappings, see below
   windownav = {        -- window movement mappings to navigate out of nnn
      left = "<C-w>h",
      right = "<C-w>l"
   },
}
