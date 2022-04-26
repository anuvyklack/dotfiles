require('lspsaga').setup {
   use_saga_diagnostic_sign = true,

   error_sign = "", --   
   warn_sign  = "", --   
   hint_sign  = "", --   
   infor_sign = "", --   
   diagnostic_header_icon = '  ', --  

   code_action_icon = ' ',
   code_action_prompt = {
      enable = true,
      sign = true,
      sign_priority = 40,
      virtual_text = false,
   },

   rename_prompt_prefix = '', -- ➤ 

   -- finder
   finder_definition_icon = '  ',
   finder_reference_icon = '  ',
   max_preview_lines = 10, -- preview lines of lsp_finder and definition preview

   definition_preview_icon = '   ',
   border_style = 'round', -- 'single', 'double', 'round', 'plus'

   --------------------- Keybindings ---------------------

   code_action_keys = {
      quit = 'q',
      exec = '<CR>'
   },
   finder_action_keys = {
      -- open = 'o', -- original
      open = '<Enter>',
      vsplit = 's', split = 'i',
      quit = {'q', '<Esc>'}, -- quit can be a table
      -- scroll_down = '<C-f>', scroll_up = '<C-b>'
   },
   rename_action_keys = {
      quit = '<C-c>', -- quit can be a table
      exec = '<CR>'
   },

   server_filetype_map = {},
   diagnostic_prefix_format = "%d. ",
   diagnostic_message_format = "%m %c",
   highlight_prefix = false,
}
