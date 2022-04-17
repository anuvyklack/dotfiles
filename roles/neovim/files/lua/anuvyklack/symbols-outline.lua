-- :lua print(vim.inspect(vim.lsp.protocol.CompletionItemKind))
vim.g.symbols_outline = {
   -- Whether to highlight the currently hovered symbol. High cpu usage.
   highlight_hovered_item = true,
   show_guides = true,

   position = 'right',
   -- position = 'left',

   -- Whether width of window is set relative to existing windows
   -- relative_width = true,
   -- Width of window (as a % or columns based on relative_width)
   width = 30,

   auto_preview = true,

   show_relative_numbers = false, -- Shows relative numbers with the outline.

   -- Shows extra details with the symbols (lsp dependent).
   show_symbol_details = true,

   -- Background color of the preview window string.
   -- preview_bg_highlight = 'Pmenu',

   -- Which symbols to ignore (possible values) table.
   symbol_blacklist = { '_' },

   lsp_blacklist = {}, -- Which lsp clients to ignore

   keymaps = {
      close = "<Esc>",
      goto_location = "<Cr>",
      focus_location = "o",
      hover_symbol = "<C-space>",
      rename_symbol = "r",
      code_actions = "a",
   },
   symbols = {
      File          = {icon = "",  hl = "TSURI"},         --  
      Module        = {icon = "󰘦",  hl = "TSNamespace"},   --   󰘦 
      Namespace     = {icon = "󰘦",  hl = "TSNamespace"},   --  󰘦
      Package       = {icon = "",  hl = "TSNamespace"},   --    
      Class         = {icon = "",  hl = "TSType"},        -- 𝓒 
      Method        = {icon = "ƒ",  hl = "TSMethod"},      -- ƒ
      Property      = {icon = "",  hl = "TSMethod"},      --  
      Field         = {icon = "",  hl = "TSField"},       --   
      Constructor   = {icon = "",  hl = "TSConstructor"}, -- 
      Enum          = {icon = "",  hl = "TSType"},        -- ℰ  烈
      Interface     = {icon = "",  hl = "TSType"},        -- ﰮ    
      Function      = {icon = " ",  hl = "TSFunction"},    --  
      Variable      = {icon = "",  hl = "TSConstant"},    --  
      Constant      = {icon = "",  hl = "TSConstant"},    -- 
      String        = {icon = "",  hl = "TSString"},      -- 𝓐 
      Number        = {icon = "",  hl = "TSNumber"},      -- # 
      Boolean       = {icon = "",  hl = "TSBoolean"},     --   ⏻
      Array         = {icon = "",  hl = "TSConstant"},    -- 
      Object        = {icon = "",  hl = "TSType"},        -- ⦿ 
      Key           = {icon = "",  hl = "TSType"},        -- 
      Null          = {icon = "",  hl = "TSType"},        -- N 
      EnumMember    = {icon = "",  hl = "TSField"},       --  
      Struct        = {icon = "",  hl = "TSType"},        -- 𝓢 
      Event         = {icon = "🗲",  hl = "TSType"},        -- 🗲
      Operator      = {icon = "",  hl = "TSOperator"},    -- + 
      TypeParameter = {icon = "𝙏",  hl = "TSParameter"}    -- 𝙏
   }
}
