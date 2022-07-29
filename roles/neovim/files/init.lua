--   ██           ██   ██       ███
--  ░░           ░░   ░██      ░░██
--  ███  ██████   ██ ██████     ░██ ██   ██  █████
-- ░░██ ░██░░░██ ░██░░░██░      ░██░██  ░██ ░░░░░██
--  ░██ ░██  ░██ ░██  ░██       ░██░██  ░██  ██████
--  ░██ ░██  ░██ ░██  ░██       ░██░██  ░██ ██░░░██
--  ░██ ░██  ░██ ░██  ░░███  ██ ░██░░█████ ░░███████
--  ░░  ░░   ░░  ░░    ░░░  ░░  ░░  ░░░░░   ░░░░░░░

local cmd = vim.cmd
local o, opt, opt_local = vim.o, vim.opt, vim.opt_local
local g = vim.g
local fn = vim.fn
local has =	function(item) return fn.has(item) == 1 end
local command = vim.api.nvim_create_user_command
local autocmd = vim.api.nvim_create_autocmd
P = vim.pretty_print

if not pcall(require, 'impatient') then
	print 'Failed to load impatient'
end

-- prequire {{{

---Protected `require` function
---@param module_name string
---@return table | function | Void module
---@return boolean loaded if module was loaded or not
function prequire(module_name)
   local available, module = pcall(require, module_name)
   if available then
      return module, true
   else
      local source = debug.getinfo(2, "S").source:sub(2)
      source = source:gsub(os.getenv('HOME')--[[@as string]], '~')
      local msg = string.format('"%s" requested in "%s" not available', module_name, source)
		vim.schedule(function() vim.notify_once(msg, vim.log.levels.WARN) end)

		---@class Void Void has eveything and nothing
		local void = setmetatable({}, { ---@type Void
			__index = function(self) return self end,
			__newindex = function() end,
			__call = function() end
		})

      return void, false
   end
end
-- }}}

-- Nvui GUI
if g.nvui then cmd 'source ~/.config/nvim/ginit.vim' end

-- Options ----------------------------------------------------------------- {{{

vim.opt.fileencodings = { 'utf-8', 'cp1251' }
o.termguicolors = true			-- 24 bit color support

-- g.mapleader = vim.api.nvim_replace_termcodes('<Space>', true, true, true)
g.mapleader = ' ' -- <Space>
g.maplocalleader = ','

opt.virtualedit = 'block'
o.mouse = 'a'
o.splitbelow = true	-- Open splits below and ...
o.splitright = true	-- ... to the right.

o.undofile = true		-- Save undo history in a file between sessions

-- o.number = true
-- o.relativenumber = true

-- o.autochdir = true	-- Set pwd as the dir of the active file.
-- 							-- WARNING: Seting this option breakes
-- 							-- `ahmedkhalf/project.nvim' plugin.

o.updatetime = 1000	-- The time until the CursorHold autocommand will triggered

o.title = true
o.titlestring = '%t'	-- tile

o.exrc = true			-- Allow vim search local configuratin files in project folders.
o.secure = true		-- Disallows the use of :autocmd, shell and write commands
							-- in local exrc files.

-- cmd 'syntax enable'

-- Turn Off Swap Files ---------------------------------------------------------

o.swapfile = false

-- Needed for coc.nvim: some LSP servers have issues with backup files,
-- see: https://github.com/neoclide/coc.nvim/issues/649
o.backup = false
o.writebackup = false

-- Formating text --------------------------------------------------------------

o.textwidth = 80
opt.formatoptions:append('21')	-- The behavior of 'gw' command.
o.joinspaces = true	-- Put two spaces after period.
o.linebreak = true	-- Wrap lines at convenient points.
o.bomb = false

-- Use Par Unix utility for 'gq' command
-- setglobal formatprg=par\ -w75\ g
-- setglobal formatprg=par\ -w80\ g
-- setglobal formatprg=par\ -w80\ g\ q
--
-- function! UpdateFormatprg()
--    let &g:formatprg = substitute( &g:formatprg , '\d\+$', &textwidth , "" )
-- endfunction
--
-- augroup UpdateFormatprgGroup
--   autocmd!
--   autocmd VimEnter,BufEnter * call UpdateFormatprg()
--   autocmd OptionSet textwidth call UpdateFormatprg()
-- augroup END

-- Folds -----------------------------------------------------------------------

o.foldcolumn = 'auto:2'
o.foldlevel = 0
o.foldlevelstart = 0	-- 0: to always start editing with all folds closed

-- o.foldnestmax = 20	-- maximum nested fold level
-- o.foldminlines = 4	-- minimum lines required to create fold
opt.foldopen:append('jump')

-- o.foldmethod = 'marker'
-- o.foldmethod = 'expr'	-- use folding based on treesitter by default
o.foldexpr = 'nvim_treesitter#foldexpr()'

-- https://github.com/neovim/neovim/issues/15670
-- opt.fillchars = { foldclose = '', foldopen = '' }
opt.fillchars = { foldclose = '', foldopen = '' }

-- Indent ----------------------------------------------------------------------

o.smartindent = true	-- does the right thing (mostly) in programs
-- o.cindent = true		-- stricter rules for C programs

o.smarttab = true		-- “Умная” расстановка отступов.
o.expandtab = true	-- Заменять табуляцию пробелами.
o.tabstop = 4			-- Количество пробелов в одном символе табуляции.
o.softtabstop = -1	-- Synchronize tabstop with shiftwidth.
o.shiftwidth = 4		-- Количество пробелов на которое будет сдвинута
							-- строка командами >> или <<.

o.list = true			-- Show invisible symbols.
opt.listchars = { tab = '› ', trail = '‧', nbsp = '+' }
-- opt.listchars = { tab = '⇥ ', trail = '‧', 'nbsp = '+' }
-- opt.listchars:append{ eol = '¤' }
-- opt.listchars"append{ eol = '¶' }

-- Visual Tweaks ---------------------------------------------------------------

o.laststatus = 3		-- Use global statusline
o.signcolumn = 'auto:1'
o.wrap = false			-- wrap lines
-- o.cmdheight = 1		-- Make command line one line high.
o.colorcolumn = '+1'	-- Показывать рулетку в следующей колонке после textwidth.
o.mousehide = true	-- Hide the mouse when typing text.
-- o.cursorline = true	-- Выделять строку, на которой находится курсор.
opt.cursorlineopt = { 'number', 'screenline' }

-- -- Change cursor shape between modes
-- hi Cursor guifg=green guibg=green
-- hi Cursor2 guifg=red guibg=red
-- set guicursor=n-v-c:block-Cursor/lCursor,i-ci-ve:ver25-Cursor2/lCursor2,r-cr:hor20,o:hor50
-- " set guicursor=i-ci-ve:ver25-Cursor2/lCursor2,r-cr:hor20,o:hor50

-- Search ----------------------------------------------------------------------

o.ignorecase = true		-- Ignore case when searching...
o.smartcase = true		-- ...unless we type a capital.
o.inccommand = 'split'	-- Shows a preview window of all the changes you are
								-- going to make in the document.

-- Completion ------------------------------------------------------------------

opt.completeopt = { 'menu', 'menuone', 'noselect' }

-- o.wildmode = 'list:longest'
							-- При автодополнении в командном режиме (:command)
							-- в начале списка показывать самый длинный вариант

opt.wildcharm = ('<C-z>'):byte()
							-- https://github.com/neovim/neovim/issues/18000
							-- Символ, который активирует автодополнение в скриптовых
							-- коммандах или при назначении клавиш.  Использование
							-- <Tab> в этих ситуациях непосредственно вставит символ
							-- табуляции, а не откроет меню дополнения.

opt.wildoptions = { 'pum', 'tagfile' }
-- opt.wildoptions = 'tagfile'
							-- Отключение опции 'pum' включает автодополнение
							-- в командной строке в старом стиле: в строку, а не
							-- в формате вертикального выпадающего списка.


-- Pop-Up Menu -----------------------------------------------------------------

-- o.pumblend = 0		-- Enables pseudo-transparency for the popup-menu.
							-- With 0 value the transparency is disabled, which is
							-- necessary for proper work of lspkind-icons plugin.
							-- Because most Nerd Font icons require 2 cells where
							-- second shold be the space for correct render.  And
							-- with pseudo transparency turned on, the underlying
							-- characters can occupy a cell required for the space
							-- character.

-- o.pumheight = 15	-- Количество строк во всплывающем окне

-- Scrolling -------------------------------------------------------------------

o.scrolloff = 0		-- Start scrolling when we're n lines away from margins.
opt_local.scrolloff = 0	-- https://github.com/karb94/neoscroll.nvim/issues/28

o.sidescrolloff = 0	-- Сколько колонок должно остаться до конца экрана,
							-- чтобы Vim начал прокручивать экран вбок.

o.sidescroll = 1		-- Минимальное количество колонок на которое экран
							-- будет прокручиваться горизонтально за раз.

-- Spelling --------------------------------------------------------------------

opt.spelllang = { 'ru_ru', 'en_us' }
-- opt.dictionary = {}	-- using dictionaries

-- Русский язык ----------------------------------------------------------------

o.keymap = 'russian-jcukenwin'
o.iminsert = 0			-- Чтобы при старте ввод был на английском, а не на русском.
o.imsearch = -1		-- Чтобы при старте поиск был на английском, а не на русском.

-- -- Менять цвет курсора при включенном русском языке
-- highlight Cursor guifg=Cyan guibg=Green
-- highlight lCursor guifg=NONE guibg=green
--
-- highlight Cursor guifg=NONE guibg=#e8ae3c
-- highlight lCursor guifg=NONE guibg=#e7ae3c

-- o.helplang = 'ru'		-- Помощь на русском языке

-- Markdown ---------------------------------------------------------------- {{{
-- https://github.com/tpope/vim-markdown
g.markdown_fenced_languages = {
	'lua', 'vim', 'shell=sh', 'bash=sh', 'cpp', 'python', 'json', 'html'
}
g.markdown_minlines = 100	-- Syntax highlight is synchronized in 100 lines.

-- g.markdown_folding = 1	-- Enable folding in markdown files. The value of the
								-- variable does not matter. It just should be set.
								-- :help ft-markdown-plugin

g.vimsyn_embed = 'lPr'	-- Turn on syntax highlighting for embeded lua, python
								-- and ruby pieces of code inside Vimscript in *.vim files.
-- }}}

-- Clipboard --------------------------------------------------------------- {{{
if has('wsl') then
	g.clipboard = {
		name = 'wslclipboard',
		copy = {
			['+'] = '/mnt/c/tools/win32yank.exe -i --crlf',
			['*'] = '/mnt/c/tools/win32yank.exe -i --crlf',
		 },
		paste = {
			['+'] = '/mnt/c/tools/win32yank.exe -o --lf',
			['*'] = '/mnt/c/tools/win32yank.exe -o --lf',
		},
		cache_enabled = 1,
	}
-- elseif has('unix') then
elseif os.getenv('XDG_SESSION_TYPE') == 'wayland' then
	-- g.clipboard = {
	-- 	name = 'wl-clipboard',
	-- 	copy = {
	-- 		['+'] = 'wl-copy',
	-- 		['*'] = 'wl-copy',
	-- 	},
	-- 	paste = {
	-- 		['+'] = 'wl-paste',
	-- 		['*'] = 'wl-paste',
	-- 	},
	-- 	cache_enabled = 1,
	-- }
	g.clipboard = {
	  name = 'xsel_override',
	  copy = {
		 ['+'] = 'xsel --input --clipboard',
		 ['*'] = 'xsel --input --primary',
	  },
	  paste = {
		 ['+'] = 'xsel --output --clipboard',
		 ['*'] = 'xsel --output --primary',
	  },
	  cache_enabled = 1,
	}
end
-- }}}

-- Python support ---------------------------------------------------------- {{{
if has('unix') then
	g.python3_host_prog = '/usr/bin/python3'

	-- Use conda environment
	-- ----------------------------------------------------------------
	-- g.python3_host_prog = '/opt/miniconda3/envs/nvim/bin/python'
elseif has('win32') then
	g.python3_host_prog = 'C:/scoop/apps/miniconda3/current/python.exe'
	g.python_host_prog	= 'C:/scoop/apps/miniconda2/current/envs/nvimpy2/python.exe'
end
-- }}}

-- Settings that unnecessary in Neovim ------------------------------------- {{{

-- o.encoding = 'utf-8'
-- opt.backspace = { 'indent', 'eol', 'start' }
-- o.showcmd = true			-- Show incomplete cmds down the bottom.
-- o.showmode = true			-- Show current mode down the bottom.
-- o.novisualbell = true	-- Отключаем пищалки и моргалки.
-- o.autoread = true			-- Reload files changed outside vim.
-- o.ttimeoutlen = 50		-- Время переключения между режимами (default 50)
-- o.timeoutlen = 1000		-- Время в мс в течении которого Vim ждёт продолжения
-- 								-- многосимвольной команды.
-- o.history = 10000			-- Store lots of :cmdline history. Defaults: 10000 (the maximum)
-- o.hidden = true			-- Buffers can exist in the background without being in
-- 								-- a window.
-- o.wildmenu = true			-- Enable ctrl-n and ctrl-p to scroll through matches.
-- o.incsearch = true		-- Подсвечивать найденный текст по мере набора.
-- o.hlsearch = true			-- Highlight searches by default.
--
-- o.autoindent = true		-- Use the current indentation when creating a new line
--									-- in Insert mode, both through normal Enter or o/O.

-- cmd 'filetype plugin indent on'
--		-- Is a short form of these commands:
--		--
--		--	  filetype on
--		--	  filetype plugin on
--		--	  filetype indent on
--		--
--		-- The first command turns on filetype detection for Vim to help set
--		-- syntax highlighting and other options. The plugin part will load
--		-- plugins for specific filetype if they exist. The last bit will load
--		-- indent file for specific filetype if they exist too.
--		--
--		-- For example, if you want to activate certain plugins for only Python
--		-- language, then you can create a file ~/.vim/ftplugin/python.vim.
--		-- Put all the plugins and commands you want specifically for Python
--		-- inside that file.
--		--
--		-- A good practice is to separate the indent configuration inside
--		-- another file (~/.vim/indent/python.vim).	However, I usually just
--		-- put the indents inside the plugin file.

-- }}}

-- }}}

-- Diagnostic -------------------------------------------------------------- {{{
--       󰂭
--     
--  
--     
--     
--    
fn.sign_define('DiagnosticSignError', { text = '', texthl = 'DiagnosticSignError' })
fn.sign_define('DiagnosticSignWarn',  { text = '', texthl = 'DiagnosticSignWarn'  })
fn.sign_define('DiagnosticSignInfo',  { text = '', texthl = 'DiagnosticSignInfo'  })
fn.sign_define('DiagnosticSignHint',  { text = '', texthl = 'DiagnosticSignHint'  })
-- }}}

-- Functions, Commands, Autocommands --------------------------------------- {{{

command('I', 'edit $MYVIMRC', { desc = 'open "init.lua" file' })
command('P', function() -- {{{
	cmd('edit '..fn.stdpath('config')..'/lua/plugins.lua')
end, { desc = 'open "plugins.lua" file' }) -- }}}

-- Highlight on yank, see ':help lua-highlight'
-- au TextYankPost * silent! lua vim.highlight.on_yank{ higroup="IncSearch", timeout=250 }
autocmd('TextYankPost', { -- {{{
	callback = function()
		vim.highlight.on_yank({ higroup = 'IncSearch', timeout = 250 })
	end,
	desc = 'highlight text on yank'
}) -- }}}

-- }}}

-- Plugins ----------------------------------------------------------------- {{{

PackerCompiled = '_packer_compiled'

-- Install Packer
local packer_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if fn.empty(fn.glob(packer_path)) > 0 then ---@diagnostic disable-line
	fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', packer_path })
	cmd 'packadd packer.nvim'
	require('plugins').sync()
end

command('Packer',        function() cmd 'packadd packer.nvim'; require('plugins')           end, {})
command('PackerInstall', function() cmd 'packadd packer.nvim'; require('plugins').install() end, {})
command('PackerUpdate',  function() cmd 'packadd packer.nvim'; require('plugins').update()  end, {})
command('PackerSync',    function() cmd 'packadd packer.nvim'; require('plugins').sync()    end, {})
command('PackerClean',   function() cmd 'packadd packer.nvim'; require('plugins').clean()   end, {})
command('PackerCompile', function() cmd 'packadd packer.nvim'; require('plugins').compile() end, {})

prequire(PackerCompiled)

-- }}}

-- vim: fdm=marker noet cc=+1 ts=3 sts=3
