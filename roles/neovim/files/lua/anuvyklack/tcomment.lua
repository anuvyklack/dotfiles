-- local available, whick_key = pcall(require, "which-key")
-- if not available then return end
--
-- whick_key.register({
--     ['gc'] = { name = 'Comments' },
--     ['gcc'] = 'Current line'
-- })

local mapx_available, mapx = pcall(require, 'mapx')
if not mapx_available then return end

mapx.nname('gc', 'Comments')
-- mapx.vname('gc', 'Comment region')
