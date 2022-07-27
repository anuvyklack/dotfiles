call wilder#setup({'modes': [':', '/', '?']})

" For Neovim or Vim with yarp
" For wild#cmdline_pipeline():
"	'language'	 : set to 'python' to use python
"	'fuzzy'		 : set fuzzy searching
" For wild#python_search_pipeline():
"	'pattern'	 : can be set to wilder#python_fuzzy_delimiter_pattern() for stricter fuzzy matching
"	'sorter'	 : omit to get results in the order they appear in the buffer
"	'engine'	 : can be set to 're2' for performance, requires pyre2 to be installed
"
" 'file_command' : for ripgrep : ['rg', '--files']
"				 : for fd	   : ['fd', '-tf']
" 'dir_command'  : for fd	   : ['fd', '-td']
" 'filters'		 : use ['cpsm_filter'] for performance, requires cpsm vim plugin
"				   found at https://github.com/nixprime/cpsm
call wilder#set_option('pipeline', [
\	wilder#branch(
\		wilder#python_file_finder_pipeline({
\			'file_command': ['find', '.', '-type', 'f', '-printf', '%P\n'],
\			'dir_command': ['find', '.', '-type', 'd', '-printf', '%P\n'],
\			'filters': ['fuzzy_filter', 'difflib_sorter'],
\		}),
\		wilder#branch(
\			wilder#cmdline_pipeline({
\				'language': 'python',
\				'fuzzy': 1,
\			}),
\			wilder#python_search_pipeline({
\				'pattern': wilder#python_fuzzy_pattern(),
\				'sorter': wilder#python_difflib_sorter(),
\				'engine': 're2',
\			}),
\		),
\	),
\ ])


" 'highlighter' : applies highlighting to the candidates
call wilder#set_option(
\	'renderer',
\	wilder#popupmenu_renderer({
\		'apply_incsearch_fix': 1,
\		'highlighter': wilder#basic_highlighter(),
\	})
\)

" " 'border' : 'single', 'double', 'rounded' or 'solid' can also be a list of 8 characters
" " 'highlights.border' : highlight to use for the border`
" call wilder#set_option('renderer', wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
" \	'highlighter': wilder#basic_highlighter(),
" \	'highlights': {
" \		'border': 'Normal',
" \	},
" \	'border': 'rounded',
" \ })))

" ----------------------------------------------------------------------------

" " Create the WilderAccent highlight by overriding the guifg attribute of Pmenu
" " and return the name of the highlight
" let l:hl = wilder#make_hl('WilderAccent', 'Pmenu', [{}, {}, {'foreground': '#f4468f'}])
"
" call wilder#set_option('renderer', wilder#popupmenu_renderer({
"		\ 'highlighter': wilder#basic_highlighter(),
"		\ 'highlights': {
"		\	'accent': l:hl,
"		\ },
"		\ }))
"
" " Can also be passed to the 'highlights' option
" call wilder#set_option('renderer', wilder#popupmenu_renderer({
"		\ 'highlighter': wilder#basic_highlighter(),
"		\ 'highlights': {
"		\	'accent': wilder#make_hl('WilderAccent', 'Pmenu', [{}, {}, {'foreground': '#f4468f'}]),
"		\ },
"		\ }))

" ----------------------------------------------------------------------------

" call wilder#setup({'modes': [':', '/', '?']})
"
" " call wilder#set_option('pipeline', [
" "		  \   wilder#branch(
" "		  \		wilder#python_file_finder_pipeline({
" "		  \		  'file_command': {_, arg -> stridx(arg, '.') != -1 ? ['fdfind', '-tf', '-H'] : ['fd', '-tf']},
" "		  \		  'dir_command': ['fd', '-td'],
" "		  \		  'filters': ['cpsm_filter'],
" "		  \		}),
" "		  \		wilder#substitute_pipeline({
" "		  \		  'pipeline': wilder#python_search_pipeline({
" "		  \			'skip_cmdtype_check': 1,
" "		  \			'pattern': wilder#python_fuzzy_pattern({
" "		  \			  'start_at_boundary': 0,
" "		  \			}),
" "		  \		  }),
" "		  \		}),
" "		  \		wilder#cmdline_pipeline({
" "		  \		  'fuzzy': 1,
" "		  \		  'fuzzy_filter': has('nvim') ? wilder#lua_fzy_filter() : wilder#vim_fuzzy_filter(),
" "		  \		}),
" "		  \		[
" "		  \		  wilder#check({_, x -> empty(x)}),
" "		  \		  wilder#history(),
" "		  \		],
" "		  \		wilder#python_search_pipeline({
" "		  \		  'pattern': wilder#python_fuzzy_pattern({
" "		  \			'start_at_boundary': 0,
" "		  \		  }),
" "		  \		}),
" "		  \   ),
" "		  \ ])
"
" call wilder#set_option('pipeline', [
"		\	wilder#branch(
"		\	  wilder#python_file_finder_pipeline({
"		\		'file_command': {_, arg -> stridx(arg, '.') != -1 ? ['fdfind', '-tf', '-H'] : ['fd', '-tf']},
"		\		'dir_command': ['fd', '-td'],
"		\		'filters': ['cpsm_filter'],
"		\	  }),
"		\	  wilder#substitute_pipeline({
"		\		'pipeline': wilder#python_search_pipeline({
"		\		  'skip_cmdtype_check': 1,
"		\		  'pattern': wilder#python_fuzzy_pattern({
"		\			'start_at_boundary': 0,
"		\		  }),
"		\		}),
"		\	  }),
"		\	  [
"		\		wilder#check({_, x -> empty(x)}),
"		\		wilder#history(),
"		\	  ],
"		\	  wilder#python_search_pipeline({
"		\		'pattern': wilder#python_fuzzy_pattern({
"		\		  'start_at_boundary': 0,
"		\		}),
"		\	  }),
"		\	),
"		\ ])
"
" " let s:highlighters = [
" "		  \ wilder#pcre2_highlighter(),
" "		  \ has('nvim') ? wilder#lua_fzy_highlighter() : wilder#cpsm_highlighter(),
" "		  \ ]
"
" let s:popupmenu_renderer = wilder#popupmenu_renderer(wilder#popupmenu_border_theme({
"		\ 'border': 'rounded',
"		\ 'empty_message': wilder#popupmenu_empty_message_with_spinner(),
"		\ 'highlighter': s:highlighters,
"		\ 'left': [
"		\	' ',
"		\	wilder#popupmenu_devicons(),
"		\	wilder#popupmenu_buffer_flags({
"		\	  'flags': ' a + ',
"		\	  'icons': {'+': '', 'a': '', 'h': ''},
"		\	}),
"		\ ],
"		\ 'right': [
"		\	' ',
"		\	wilder#popupmenu_scrollbar(),
"		\ ],
"		\ }))
"
" " let s:wildmenu_renderer = wilder#wildmenu_renderer({
" "		  \ 'highlighter': s:highlighters,
" "		  \ 'separator': ' · ',
" "		  \ 'left': [' ', wilder#wildmenu_spinner(), ' '],
" "		  \ 'right': [' ', wilder#wildmenu_index()],
" "		  \ })
"
" let s:wildmenu_renderer = wilder#wildmenu_renderer({
"		\ 'highlighter': wilder#basic_highlighter(),
"		\ 'separator': ' · ',
"		\ 'left': [' ', wilder#wildmenu_spinner(), ' '],
"		\ 'right': [' ', wilder#wildmenu_index()],
"		\ })
"
" call wilder#set_option('renderer', wilder#renderer_mux({
"		\ ':': s:popupmenu_renderer,
"		\ '/': s:wildmenu_renderer,
"		\ 'substitute': s:wildmenu_renderer,
"		\ }))
