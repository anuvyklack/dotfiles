local util = require('util')
local keymap = util.keymap
local cmd = keymap.cmd
local which_key = util.which_key

keymap.set('n', 'n', cmd("execute('normal! '..v:count1..'n')")..cmd("lua require('hlslens').start()"))
keymap.set('n', 'N', cmd("execute('normal! '..v:count1..'N')")..cmd("lua require('hlslens').start()"))

keymap.set('n', '*', "*"..cmd("lua require('hlslens').start()"))
keymap.set('n', '#', "#"..cmd("lua require('hlslens').start()"))

keymap.set('n', 'g*', "g*"..cmd("lua require('hlslens').start()"))
keymap.set('n', 'g#', "g#"..cmd("lua require('hlslens').start()"))

which_key.register({
   ["g*"] = "which_key_ignore",
   ["g#"] = "which_key_ignore",
})
