-- Source: https://github.com/tummetott/unimpaired.nvim

function blank_above()
   local repeated = vim.fn["repeat"]({ "" }, vim.v.count1)
   local line = vim.api.nvim_win_get_cursor(0)[1]
   vim.api.nvim_buf_set_lines(0, line - 1, line - 1, true, repeated)
end

function blank_below()
   local repeated = vim.fn["repeat"]({ "" }, vim.v.count1)
   local line = vim.api.nvim_win_get_cursor(0)[1]
   vim.api.nvim_buf_set_lines(0, line, line, true, repeated)
end

return {}
