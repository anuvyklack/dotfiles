require('bufferline').setup {
   options = {

      numbers = "none",  -- "none" | "ordinal" | "buffer_id" | "both"

      -- buffer_id at index 1, ordinal at index 2
      number_style = "superscript", -- "superscript" | "" | { "none", "subscript" }

      mappings = false, -- true | false,

      -- Mouse actions
      -- can be a string | function
      -- see ":help Mouse actions"
      close_command = "bdelete! %d",
      right_mouse_command = "bdelete! %d",
      left_mouse_command = "buffer %d",
      middle_mouse_command = nil,

      -- -- NOTE: this plugin is designed with this icon in mind,
      -- -- and so changing this is NOT recommended, this is intended
      -- -- as an escape hatch for people who cannot bear it for whatever reason
      -- -- indicator_icon = '▎',
      -- indicator_icon = '▎',
      -- buffer_close_icon = '',
      -- modified_icon = '●',
      -- close_icon = '',
      -- left_trunc_marker = ' ',
      -- right_trunc_marker = ' ',
      -- max_name_length = 30,
      -- max_prefix_length = 15, -- prefix used when a buffer is de-duplicated
      -- tab_size = 30,

      -- diagnostics = false,  -- false | "nvim_lsp",
      -- diagnostics_indicator = function(count, level, diagnostics_dict, context)
      --   return "("..count..")"
      -- end,

      -- -- NOTE: this will be called a lot so don't do any heavy processing here
      -- custom_filter = function(buf_number)
      --     -- filter out filetypes you don't want to see
      --     if vim.bo[buf_number].filetype ~= "<i-dont-want-to-see-this>" then
      --       return true
      --     end
      --
      --     -- filter out by buffer name
      --     if vim.fn.bufname(buf_number) ~= "<buffer-name-I-dont-want>" then
      --       return true
      --     end
      --
      --     -- filter out based on arbitrary rules
      --     -- e.g. filter out vim wiki buffer from tabline in your work repo
      --     if vim.fn.getcwd() == "<work-repo>" and vim.bo[buf_number].filetype ~= "wiki" then
      --       return true
      --     end
      -- end,

      offsets = {
         filetype = "NvimTree",
         text = "File Explorer",
         text_align = "center", -- "left" | "center" | "right"
      },

      -- disable filetype icons for buffers
      show_buffer_icons = true,

      show_buffer_close_icons = true,
      show_close_icon = true,
      show_tab_indicators = true,
      persist_buffer_sort = true, -- whether or not custom sorted buffers should persist

      -- can also be a table containing 2 custom separators
      -- [focused and unfocused]. eg: { '|', '|' }
      separator_style = "slant",  -- "slant" | "thick" | "thin" | { 'any', 'any' },

      enforce_regular_tabs = true,
      always_show_bufferline = true,

      -- sort_by = 'extension' | 'relative_directory' | 'directory' | function(buffer_a, buffer_b)
      --   -- add custom logic
      --   return buffer_a.modified > buffer_b.modified
      -- end
      sort_by = 'relative_directory',
   },
}
