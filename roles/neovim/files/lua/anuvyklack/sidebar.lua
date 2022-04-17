require("sidebar-nvim").setup {
   -- initial_width = 50,
   hide_statusline = true,
   sections = { 'files', 'symbols', 'todos' },
   containers = {
      attach_shell = "/bin/sh", show_all = true, interval = 5000,
   },
   datetime = { format = "%a %b %d, %H:%M", clocks = { { name = "local" } } },
   todos = { ignored_paths = { "~" } },
}
