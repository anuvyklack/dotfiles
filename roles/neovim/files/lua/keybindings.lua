--   ██                           ██      ██              ██ ██
--  ░██                          ░██     ░░              ░██░░
--  ░██   ██  █████  ██   ██     ░██████  ██ ██████   ██████ ██ ██████   ██████  ██████
--  ░██  ██  ██░░░██░██  ░██     ░██░░░██░██░██░░░██ ██░░░██░██░██░░░██ ██░░░██ ██░░░░
--  ░█████  ░███████░██  ░██     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░██  ░██░░█████
--  ░██░░██ ░██░░░░ ░░██████     ░██  ░██░██░██  ░██░██  ░██░██░██  ░██░░██████ ░░░░░██
--  ░██ ░░██░░█████  ░░░░░██     ░██████ ░██░██  ░██░░██████░██░██  ░██ ░░░░░██ ██████
--  ░░   ░░  ░░░░░    █████      ░░░░░░  ░░ ░░   ░░  ░░░░░░ ░░ ░░   ░░   █████ ░░░░░░
--                   ░░░░░                                              ░░░░░

local is_module_available = require("utility").is_module_available
-- Using as: 'if is_module_available("menu") then require("menu") end'

-- All functions that need to be exported should go in this table.
local _M = {}

-- -- Dealing with word wrap:
-- -- If cursor is inside very long line in the file than wraps around
-- -- several rows on the screen, then 'j' key moves you to the next line
-- -- in the file, but not to the next row on the screen under your
-- -- previous position as in other editors.  These bindings fixes this.
-- vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", {noremap = true, expr = true, silent = true})
-- vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", {noremap = true, expr = true, silent = true})


function _M.lspconfig (bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

    -- Mappings options.
    local opts = { noremap=true, silent=false }

    -- Default lspconfig bindings {{{

    -- -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap('n', '<F2>', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', '<S-F2>', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    -- buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    -- buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    -- buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    -- buf_set_keymap('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    -- buf_set_keymap('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    -- buf_set_keymap('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
    buf_set_keymap('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    -- buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    -- buf_set_keymap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    -- buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    -- buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    -- buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    -- buf_set_keymap('n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    -- buf_set_keymap("n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

    --}}}

    -- Lspsaga {{{

    -- lsp provider to find the cursor word definition and reference
    buf_set_keymap("n", "gd", ":Lspsaga lsp_finder<CR>", opts)

    -- code action
    buf_set_keymap("n", "<leader>ca", ":Lspsaga code_action<CR>", opts)
    buf_set_keymap("v", "<leader>ca", ":<C-U>Lspsaga range_code_action<CR>", opts)

    -- rename
    -- buf_set_keymap("n", "gr", ":Lspsaga rename<CR>", opts)
    buf_set_keymap("n", "<leader>rn", ":Lspsaga rename<CR>", opts)

    -- preview definition
    buf_set_keymap("n", "<leader>pd", ":Lspsaga preview_definition<CR>", opts)

    -- hover doc
    buf_set_keymap("n", "K", ":Lspsaga hover_doc<CR>", opts)

    -- show signature help
    buf_set_keymap("n", "<C-k>", ":Lspsaga signature_help<CR>", opts)

    -- Show Diagnostics
    -- buf_set_keymap("n", "<leader>e", ":Lspsaga show_line_diagnostics<CR>", opts)
    -- only show diagnostic if cursor is over the area
    buf_set_keymap("n", "<leader>cc", "<cmd>lua require'lspsaga.diagnostic'.show_cursor_diagnostics()<CR>", opts)

    -- jump diagnostic
    buf_set_keymap("n", "]e", ":Lspsaga diagnostic_jump_next<CR>", opts)
    buf_set_keymap("n", "[e", ":Lspsaga diagnostic_jump_prev<CR>", opts)


    -- scroll down / up inside different preview windows
    buf_set_keymap("n", "<C-f>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>", opts)
    buf_set_keymap("n", "<C-b>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>", opts)

    --}}}

end


-- Treesitter textobjects
function _M.treesitter_textobjects()
  return {
    select = { --{{{
      enable = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        -- ['aC'] = '@class.outer',
        -- ['iC'] = '@class.inner',
        ['aC'] = '@conditional.outer',
        ['iC'] = '@conditional.inner',
        ['ic'] = '@comment',
        -- ['ic'] = '@comment.inner',
        -- ['ac'] = '@comment.outer',
        ['ae'] = '@block.outer',
        ['ie'] = '@block.inner',
        ['al'] = '@loop.outer',
        ['il'] = '@loop.inner',
        ['is'] = '@statement.inner',
        ['as'] = '@statement.outer',
        ['am'] = '@call.outer',
        ['im'] = '@call.inner',

        -- -- or you use the queries from supported languages with textobjects.scm
        -- ['iF'] = {
        --   python = '(function_definition) @function',
        --   cpp = '(function_definition) @function',
        --   c = '(function_definition) @function',
        --   java = '(method_declaration) @function'
        -- }
      }
    }, --}}}
    move = { --{{{
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    }, --}}}
  }
end


return _M
