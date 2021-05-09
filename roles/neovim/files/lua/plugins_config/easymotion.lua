local g = vim.g

-- If 0 disable all the messages the plugin creates, such as
-- "EasyMotion: Jumping to [l,c]" and "EasyMotion: Cancelled".
g.EasyMotion_verbose = 1

-- Use uppercase target labels and type as a lower case
g.EasyMotion_use_upper = 1

-- type `l` and match `l` & `L`
g.EasyMotion_smartcase = 1

-- Smartsign (type `3` and match `3` & `#`)
g.EasyMotion_use_smartsign_us = 1

-- При перемещении по строкам курсор будет прыгать не в
-- начало строки, а в туже колонку, что и был.
g.EasyMotion_startofline = 0
