require("indent_blankline").setup({
   filetype = {
      'python',
      -- 'lua'
   },
   use_treesitter = true,
   show_current_context = true,
   show_current_context_start = false,
   show_end_of_line = true,
   context_highlight_list = {'Error', 'Warning'},
   show_first_indent_level = false,
   max_indent_increase = 1,
   show_trailing_blankline_indent = false,
})
