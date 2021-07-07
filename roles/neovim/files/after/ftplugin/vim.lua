--------------------- Keybindings ---------------------

local buf_set_keymap = require("util").buf_set_keymap
local opt = {noremap=true, silent=false}

buf_set_keymap(0, 'n', 'gK', 'Show :help', 'K', opt)
buf_set_keymap(0, 'v', 'gK', 'Show :help', 'K', opt)

-------------------------------------------------------
