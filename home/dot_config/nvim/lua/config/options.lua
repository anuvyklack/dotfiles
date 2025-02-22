-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

local opt = vim.opt

-- Options ---------------------------------------------------------------------

opt.relativenumber = false

opt.fileencodings:append("cp1251")
opt.virtualedit = "block"
opt.mouse = "a"
opt.updatetime = 1000 -- The time until the CursorHold autocommand will triggered

opt.shell = "/usr/bin/fish"

-- Allow vim search local configuratin files in project folders.
opt.exrc = true
-- Disallows the use of :autocmd, shell and write commands in local exrc files.
opt.secure = true

opt.jumpoptions = "view"

-- https://github.com/neovim/neovim/pull/14537
opt.diffopt:append("linematch:60")

-- Formating text --------------------------------------------------------------

opt.textwidth = 80
opt.formatoptions:append("21") -- The behavior of 'gw' command.
opt.joinspaces = false -- Put two spaces after period.
opt.linebreak = true -- Wrap lines at convenient points.
opt.bomb = false

-- LazyVim auto format
vim.g.autoformat = false

-- Use Par Unix utility for 'gq' command
-- setglobal formatprg=par\ -w75\ g
-- setglobal formatprg=par\ -w80\ g
-- setglobal formatprg=par\ -w80\ g\ q

-- Folds -----------------------------------------------------------------------

opt.foldcolumn = "auto:2"
opt.foldlevel = 0
opt.foldlevelstart = 0 -- 0: to always start editing with all folds closed
-- opt.foldnestmax = 20	-- maximum nested fold level
-- opt.foldminlines = 4	-- minimum lines required to create fold
opt.foldopen:append("jump")

-- https://github.com/neovim/neovim/issues/15670
-- opt.fillchars = { foldclose = "", foldopen = "" }

-- Indentation -----------------------------------------------------------------

opt.smartindent = true -- does the right thing (mostly) in programs

opt.smarttab = true -- “Умная” расстановка отступов.
opt.expandtab = true -- Заменять табуляцию пробелами.
opt.tabstop = 4 -- Количество пробелов в одном символе табуляции.
opt.softtabstop = -1 -- Synchronize tabstop with shiftwidth.

-- Количество пробелов на которое будет сдвинута строка командами >> или <<.
opt.shiftwidth = 4

opt.list = true -- Show invisible symbols.
opt.listchars = { tab = "› ", trail = "‧", nbsp = "+" }
-- opt.listchars = { tab = '⇥ ', trail = '‧', 'nbsp = '+' }
-- opt.listchars:append{ eol = '¤' }
-- opt.listchars"append{ eol = '¶' }

-- Visual Tweaks ---------------------------------------------------------------

opt.laststatus = 3 -- Use global statusline
-- opt.signcolumn = "auto:1"
opt.wrap = false -- wrap lines
-- opt.cmdheight = 1 -- Make command line one line high.
opt.colorcolumn = "+1" -- Показывать рулетку в следующей колонке после textwidth.
opt.mousehide = true -- Hide the mouse when typing text.
-- opt.cursorline = true -- Выделять строку, на которой находится курсор.
opt.cursorlineopt = { "number", "screenline" }

-- Search ----------------------------------------------------------------------

opt.ignorecase = true -- Ignore case when searching...
opt.smartcase = true -- ...unless we type a capital.

-- Shows a preview window of all the changes you are going to make in the
-- document.
opt.inccommand = "nosplit" -- "split"

-- Completion ------------------------------------------------------------------

opt.completeopt = { "menu", "menuone", "noselect" }

-- -- При автодополнении в командном режиме (:command) в начале списка показывать
-- -- самый длинный вариант
-- opt.wildmode = "list:longest"

-- Символ, который активирует автодополнение в скриптовых коммандах или при
-- назначении клавиш. Использование <Tab> в этих ситуациях непосредственно
-- вставит символ табуляции, а не откроет меню дополнения.
-- https://github.com/neovim/neovim/issues/18000
opt.wildcharm = ("<C-z>"):byte()

-- Отключение опции 'pum' включает автодополнение в командной строке в старом
-- стиле: в строку, а не в формате вертикального выпадающего списка.
opt.wildoptions = { "pum", "tagfile" }
-- opt.wildoptions = 'tagfile'

-- Pop-Up Menu -----------------------------------------------------------------

-- Enables pseudo-transparency for the popup-menu. With 0 value the transparency
-- is disabled, which is necessary for proper work of lspkind-icons plugin.
-- Because most Nerd Font icons require 2 cells where second shold be the space
-- for correct render. And with pseudo transparency turned on, the underlying
-- characters can occupy a cell required for the space character.
-- opt.pumblend = 0

-- o.pumheight = 15	-- Количество строк во всплывающем окне

-- Scrolling -------------------------------------------------------------------

opt.scrolloff = 0 -- Start scrolling when we're n lines away from margins.
-- vim.opt_local.scrolloff = 0 -- https://github.com/karb94/neoscroll.nvim/issues/28

-- Сколько колонок должно остаться до конца экрана, чтобы Vim начал прокручивать
-- экран вбок.
-- opt.sidescrolloff = 0

-- Минимальное количество колонок на которое экран будет прокручиваться
-- горизонтально за раз.
-- opt.sidescroll = 1

-- vim: fdm=marker noet cc=+1 ts=3 sts=3
