-- How to write snippents : https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md
-- https://github.com/L3MON4D3/LuaSnip/wiki/Cool-Snippets#all---pairs
-- https://github.com/L3MON4D3/LuaSnip/wiki/Nice-Configs
local types = require('luasnip.util.types')

require('luasnip').config.setup({
   history = false,

   -- Do not jump to snippet if i'm outside of it.
   -- https://github.com/L3MON4D3/LuaSnip/issues/78
	region_check_events = 'CursorMoved', -- 'InsertEnter',
	delete_check_events = 'TextChanged,TextChangedI',

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
})

