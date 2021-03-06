require('lspkind').init({
   with_text = true, -- enables text annotations

   -- default symbol map, can be either 'default' or 'codicons' for
   -- codicon preset (requires vscode-codicons font installed)
   --
   preset = 'default',
   -- preset = 'codicons',

   --    襁
   -- 
   --   
   --   
   -- 廓卑喝
   --   
   --      
   --  
   -- 
   --     
   --     
   --      
   --       
   --  
   --    
   --        
   --                  
   --      
   --         
   --              ﬦ ﴫ
   -- 﫻      車濫櫓 綠
   -- ﮆ 不 便 礪ﮋ 了
   --  ﰟ   
   -- 卑 喝 慎 ﲋ ﰪ    
   --    
   -- 
   --  擄
   --  
   --    
   -- 謹 鸞
   --      
   --    
   --       
   --    
   --                

   --      
   --

   symbol_map = {
      Text          = '', --             ﮝ
      Buffer        = '', --
      Method        = 'ƒ', -- ƒ
      Function      = '', --   
      Constructor   = '', --  
      Field         = "", --      
      Variable      = '', -- 
      Class         = '', --    囹
      Interface     = '', --   
      Module        = '', -- 
      Property      = '襁',--  襁
      Unit          = '', --  
      Value         = '', -- 
      Enum          = '烈',-- 烈
      Keyword       = '', -- 
      Snippet       = '﬌', -- ﬌
      Color         = '', -- 
      File          = '', -- 
      Reference     = "", --  
      Folder        = '', -- 
      EnumMember    = '', -- 
      Constant      = '', -- 
      Struct        = '', -- 
      Event         = "", -- 
      Operator      = "",
      TypeParameter = "",
   },

   -- -- override preset symbols
   -- symbol_map = {
   --     Text = "",
   --     Method = " ",
   --     Function = " ",
   --     Constructor = " ",
   --     Field = " ",
   --     Variable = " ",
   --     Class = " ",
   --     Interface = " ",
   --     Module = " ",
   --     Property = " ",
   --     Unit = "",
   --     Value = "",
   --     Enum = "",
   --     Keyword = " ",
   --     Snippet = "  ",
   --     Color = "",
   --     File = " ",
   --     Reference = "",
   --     Folder = "",
   --     EnumMember = "",
   --     Constant = " ",
   --     Struct = "",
   --     Event = "",
   --     Operator = "",
   --     TypeParameter = "",
   -- },

   -- require('vim.lsp.protocol').CompletionItemKind = {
   --       '  Text';          -- = 1
   --       '  Function';      -- = 2;
   --       '  Method';        -- = 3;
   --       '  Constructor';   -- = 4;
   --       '  Field';         -- = 5;
   --       '  Variable';      -- = 6;
   --       '  Class';         -- = 7;
   --       '  Interface';     -- = 8;
   --       '  Module';        -- = 9;
   --       '  Property';      -- = 10;
   --       '  Unit';          -- = 11;
   --       '  Value';         -- = 12;
   --       '  Enum';          -- = 13;
   --       '  Keyword';       -- = 14;
   --       '  Snippet';       -- = 15;
   --       '  Color';         -- = 16;
   --       '  File';          -- = 17;
   --       '  Reference';     -- = 18;
   --       '  Folder';        -- = 19;
   --       '  EnumMember';    -- = 20;
   --       '  Constant';      -- = 21;
   --       '  Struct';        -- = 22;
   --       '  Event';         -- = 23;
   --       '  Operator';      -- = 24;
   --       '  TypeParameter'; -- = 25;
   -- }

})
