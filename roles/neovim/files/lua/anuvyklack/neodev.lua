require('neodev').setup({
   library = {
     enabled = true,
     runtime = true, -- runtime path
     types = true, -- full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
     -- plugins = true,
     plugins = { 'plenary.nvim' },
   },

   override = function(root_dir, library)
      if root_dir:match('windows.nvim') then
         library.enabled = true
         library.runtime = true
         library.types = true
         library.plugins = { 'animation.nvim' }

      elseif root_dir:match('breadcrumbs.nvim') then
         library.enabled = true
         library.runtime = true
         library.types = true
         library.plugins = { 'nvim-api-wrappers' }
      end
   end,
})

