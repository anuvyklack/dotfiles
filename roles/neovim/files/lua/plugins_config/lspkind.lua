require('lspkind').init({
   -- default symbol map
   -- can be either 'default' (requires nerd-fonts font) or
   -- 'codicons' for codicon preset (requires vscode-codicons font)
   preset = 'default',
   -- preset = 'codicons',

   symbol_map = {
      Text          = '', -- 
      Method        = 'ƒ', -- ƒ
      Function      = '', --   
      Constructor   = '', -- 
      Field         = "", -- 
      Variable      = '', -- 
      Class         = '', -- 

      -- f12e:   (puzzle-piece)
      -- f1e6:   (plug) Font Awesome 5 Pro Solid
      Interface     = '', --    

      Module        = '󰘦', --  󰘦
      Property      = '', -- 

      -- f545:   (ruler) Font Awesome 5 Pro Solid
      Unit          = '',-- 塞 

      Value         = '', -- 
      Enum          = '', -- 
      Keyword       = '', -- 
      Snippet       = '', --   
      Color         = '', -- 
      File          = '', -- 
      Reference     = '', -- 
      Folder        = '', -- 
      EnumMember    = '', -- 
      Constant      = '', -- 

      -- f5fd:   (layer-group) Font Awesome 5 Pro
      Struct        = '', --   

      Event         = '', -- 
      Operator      = '', -- 
      TypeParameter = '𝙏', -- 𝙏
   },
})
