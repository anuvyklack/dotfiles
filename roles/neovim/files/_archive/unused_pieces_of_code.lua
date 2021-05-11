-- A snippet from the docs:

-- To set a boolean toggle:
-- In vimL:
set number

-- In Lua:
vim.opt.number = true

-- To set an array of values:
-- In vimL:
set wildignore=*.o,*.a,__pycache__

-- In Lua, there are two ways you can do this now. One is very similar to
-- the vimL way:
vim.opt.wildignore = '*.o,*.a,__pycache__'

-- However, vim.opt also supports a more elegent way of setting
-- list-style options, but using lua tables:
vim.opt.wildignore = { '*.o', '*.a', '__pycache__' }

