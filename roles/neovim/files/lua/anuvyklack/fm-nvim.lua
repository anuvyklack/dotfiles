require('fm_nvim').setup{
   -- Border around floating window.
   -- opts: 'rounded'; 'double'; 'single'; 'solid'; 'shawdow'
   border = 'rounded',

   -- Percentage (0.8 = 80%)
   height = 0.8,
   width  = 0.73,

   -- Command used to open files.
   -- opts: 'tabedit'; 'split'; 'pedit'; etc...
   edit_cmd = "edit",

   -- -- Terminal commands used w/ file manager
   -- lf_cmd     = "lf", -- eg: lf_cmd = "lf -command 'set hidden'"
   -- fm_cmd     = "fm",
   -- nnn_cmd    = "nnn",
   -- xplr_cmd   = "xplr",
   -- vifm_cmd   = "vifm",
   -- ranger_cmd = "ranger",

   -- Mappings used inside the floating window
   mappings = {
      vert_split = "<C-v>",
      horz_split = "<C-h>",
      tabedit    = "<C-t>",
      edit       = "<C-e>"
   }
}
