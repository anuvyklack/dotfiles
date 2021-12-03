require('lspsaga').init_lsp_saga {

  --    襁
  -- 
  --   
  --   
  -- 廓卑喝
  --   
  --      
  --  
  -- 

  --   diff
  --   diff added
  --   diff ignored
  --   diff modified
  --   diff removed
  --   diff renamed

  -- != <=

  -- This symbols take two places if the could.
  --    
  --    
  -- 
  --   
  --     
  --   
  --   
  -- 
  -- 
  -- 
  -- ⚠️

  -- use_saga_diagnostic_sign = true,

  -- This symbols take two places if the could.
  error_sign = "", -- 
  warn_sign  = "", -- 
  hint_sign  = "", -- 
  infor_sign = "", -- 

  diagnostic_header_icon = ' ',

  -- code action
  code_action_icon = '',
  code_action_prompt = {
    enable = true,
    sign = true,
    sign_priority = 20,
    virtual_text = true,
  },

  rename_prompt_prefix = '➤',

  -- finder
  finder_definition_icon = '  ',
  finder_reference_icon = '  ',
  max_preview_lines = 10, -- preview lines of lsp_finder and definition preview

  definition_preview_icon = '  ',
  border_style = "single", -- "single", "double", "round", "plus"


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

}
