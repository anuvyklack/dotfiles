local available, trouble = pcall(require, "trouble")
if not available then return end

trouble.setup {
   height = 10,  -- height of the trouble list when position is top or bottom
   width = 50,   -- width of the list when position is left or right

   -- "workspace_diagnostics", "document_diagnostics", "quickfix", "lsp_references", "loclist"
   mode = "workspace_diagnostics",

   action_keys = { -- key mappings for actions in the trouble list
      -- map to {} to remove a mapping, for example:
      -- close = {},
      close = "q", -- close the list
      cancel = "<esc>", -- cancel the preview and get back to your last window / buffer / cursor
      refresh = "r", -- manually refresh
      jump = {"<cr>", "<tab>"},  -- jump to the diagnostic or open / close folds
      open_split  = { "<c-x>" }, -- open buffer in new split
      open_vsplit = { "<c-v>" }, -- open buffer in new vsplit
      open_tab    = { "<c-t>" }, -- open buffer in new tab
      jump_close = {"o"}, -- jump to the diagnostic and close the list
      toggle_mode = "m",  -- toggle between "workspace" and "document" diagnostics mode
      toggle_preview = "P", -- toggle auto_preview
      hover = "K",   -- opens a small popup with the full multiline message
      preview = "p", -- preview the diagnostic location
      close_folds = {"zM", "zm"}, -- close all folds
      open_folds  = {"zR", "zr"}, -- open all folds
      toggle_fold = {"zA", "za"}, -- toggle fold of current file
      previous = "k", -- preview item
      next = "j"      -- next item
   },

   indent_lines = true, -- Add an indent guide below the fold icons,
   auto_open = false,   -- Automatically open the list when you have diagnostics,
   auto_close = false,  -- Automatically close the list when you have no diagnostics,
   auto_preview = true, -- Automatyically preview the location of the diagnostic.
                        --  <Esc> to close preview and go back to last window.
   auto_fold = false,   -- Automatically fold a file trouble list at creation.

   -- Enabling this will use the signs defined in your lsp client.
   use_lsp_diagnostic_signs = false,

   -- icons / text used for a diagnostic
   signs = {
      error       = "", -- 
      warning     = "", -- 
      hint        = "", -- 
      information = "", -- 
      other       = "﫠" -- 﫠
   }
}

require('keymaps').trouble()
