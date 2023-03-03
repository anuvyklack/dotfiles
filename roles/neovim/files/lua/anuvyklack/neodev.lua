require('neodev').setup({
   library = {
     enabled = true,
     runtime = true, -- runtime path
     types = true, -- full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
     -- plugins = true,
     plugins = {
        'plenary.nvim',
        'ufo',
        'nvim-api-wrappers',
        'cmp-nvim-lsp',
     },
   },
   override = function(root_dir, library)
      if root_dir:find('neovim-plugins', 1, true) then
         library.enabled = true
         library.runtime = true
         library.types = true
         library.plugins = {
            'animation.nvim',
            'navic',
            'nvim-api-wrappers',
            'lush.nvim'
         }
      end
   end,
})

