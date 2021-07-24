vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    auto_preview = false,
    position = 'right',
    -- position = 'left',
    show_numbers = false,
    show_relative_numbers = false,
    show_symbol_details = true,
    keymaps = {
        close = "<Esc>",
        goto_location = "<Cr>",
        focus_location = "o",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a",
    },
    lsp_blacklist = {},
    symbols = {
        File          = {icon = "",    hl = "TSURI"},         -- 
        Module        = {icon = "",    hl = "TSNamespace"},   --  
        Namespace     = {icon = "",    hl = "TSNamespace"},   -- 
        Package       = {icon = "",    hl = "TSNamespace"},   --  
        Class         = {icon = "𝓒",    hl = "TSType"},        -- 𝓒 
        Method        = {icon = "ƒ",    hl = "TSMethod"},      -- ƒ
        Property      = {icon = "",    hl = "TSMethod"},      --  
        Field         = {icon = "",    hl = "TSField"},       --   
        Constructor   = {icon = "",    hl = "TSConstructor"}, -- 
        Enum          = {icon = "烈",   hl = "TSType"},        -- ℰ烈
        Interface     = {icon = "",    hl = "TSType"},        -- 
        Function      = {icon = "",    hl = "TSFunction"},    -- 
        Variable      = {icon = "",    hl = "TSConstant"},    --  
        Constant      = {icon = "",    hl = "TSConstant"},    -- 
        String        = {icon = "𝓐",    hl = "TSString"},      -- 𝓐
        Number        = {icon = "",    hl = "TSNumber"},      -- #  濫
        Boolean       = {icon = "⊨",    hl = "TSBoolean"},     -- ⊨
        Array         = {icon = "",    hl = "TSConstant"},    -- 
        Object        = {icon = "⦿",    hl = "TSType"},        -- ⦿
        Key           = {icon = "",    hl = "TSType"},        --     🔐
        Null          = {icon = "NULL", hl = "TSType"},
        EnumMember    = {icon = "",    hl = "TSField"},       -- 
        Struct        = {icon = "𝓢",    hl = "TSType"},        -- 𝓢 
        Event         = {icon = "🗲",    hl = "TSType"},        -- 🗲
        Operator      = {icon = "",    hl = "TSOperator"},    -- +   璉 喝
        TypeParameter = {icon = "𝙏",    hl = "TSParameter"}    -- 𝙏
    }
}
