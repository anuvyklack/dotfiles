-- https://github.com/L3MON4D3/LuaSnip
-- https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md
-- https://github.com/L3MON4D3/LuaSnip/wiki/Cool-Snippets#all---pairs
-- https://github.com/L3MON4D3/LuaSnip/wiki/Nice-Configs

local types = require('luasnip.util.types')

require('luasnip').config.setup {
   history = true,
	region_check_events = 'InsertEnter',
	delete_check_events = 'User None',

   ext_opts = {
      [types.choiceNode] = {
         active = {
            virt_text = {
               {'●', 'Orange'}
            }
         }
      },
      [types.insertNode] = {
         active = {
            virt_text = {
               {'●', 'Blue'}
               -- {'●', 'Aqua'}
            }
         }
      }
   },
}

require('keybindings').luasnip()

-- vim: fdc=1
