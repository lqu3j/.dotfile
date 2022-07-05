require('plugins')
require('keymappings')
require('settings')

vim.o.completeopt = 'menuone,noinsert,noselect'

require'nvim-tree'.setup({
    update_cwd  = true,
    view = {
        width = 30,
        height = 30,
        hide_root_folder = false,
        side = 'left',
        mappings = {
            custom_only = false,
            list = {}
        },
        number = false,
        relativenumber = false,
        signcolumn = "yes"
    },
    update_focused_file = {
        enable = true,
        update_cwd = false,
        ignore_list = {},
    },
    actions = {
        change_dir = {
            global = false,
        },
        open_file = {
            quit_on_open = false,
        }
    }
})

local treesitter = require('nvim-treesitter')

local function treelocation()
    return treesitter.statusline({
        indicator_size = 70,
        type_patterns = {'class', 'function', 'method'},
        separator = ' -> '
    })
end

require'nvim-treesitter.configs'.setup {
  ensure_installed = {'lua', 'go', 'javascript', 'json','html'},
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

vim.cmd[[autocmd VimEnter * highlight clear SignColumn]]
vim.cmd[[command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!]]

vim.api.nvim_set_keymap('n', '<Leader>ff', [[<Cmd>lua require('telescope.builtin').find_files()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fr', [[<Cmd>lua require('telescope.builtin').oldfiles()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fp', [[<Cmd>lua require('telescope.builtin').git_files()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fg', [[<Cmd>lua require('telescope.builtin').live_grep()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fb', [[<Cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true, silent = true }) 
vim.api.nvim_set_keymap('n','<C-p>',":lua require'telescope'.extensions.project.project{}<CR>",{noremap = true, silent = true})

local lsp = require('lspconfig')
local util = require('lspconfig/util')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- passing in a table with on_attach function
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  -- Mappings.
  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', '<Leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<Leader>fs', [[<Cmd>lua require('telescope.builtin').lsp_dynamic_workspace_symbols()<CR>]], opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>zz', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>zz', opts)
  buf_set_keymap('n', '<Leader>xx', '<cmd>lua vim.diagnostic.setloclist()<cr>', opts)
end

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

lsp.gopls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    init_options = {
        usePlaceholders = true,
    },
    root_dir = util.root_pattern("go.mod", ".git"),
    settings = {
        -- gopls = {
        --     analyses = {
        --         unusedparams = true,
        --     },
        -- }
    }

})

require'lspconfig'.tsserver.setup{
    on_attach = on_attach,
    capabilities = capabilities,
}


require'lspconfig'.vuels.setup{
    on_attach = on_attach,
    capabilities = capabilities,
}

require('nvim-autopairs').setup({
  disable_filetype = { "TelescopePrompt" , "vim"},
})

vim.o.lazyredraw = true
vim.g.nvim_tree_respect_buf_cwd = true

require('lualine').setup({
    options = {theme = 'gruvbox'},
    sections = {
        lualine_b = {
            { 'filename', path = 1},
            { 'diagnostics'},
        },
        lualine_c = {{treelocation}},
        lualine_x = {'encoding', 'branch', 'filetype'},
    },
    tabline = {
      lualine_a = {'buffers'},
    },
})
require('Comment').setup()

local cmp = require('cmp')
local lspkind = require('lspkind')
cmp.setup({
    preselect = cmp.PreselectMode.None, 
     snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },
     mapping = {
      ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),
      ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
      ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    },
    sources = cmp.config.sources(
    {
      { name = 'nvim_lsp', trigger_characters = {'.'} },
      { name = 'vsnip' },
    },
    {
      { name = 'buffer' },
      { name = 'path'},
    }),
  formatting = {
    format = lspkind.cmp_format({
      mode = 'symbol', -- show only symbol annotations
      maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)

      -- The function below will be called before any actual modifications from lspkind
      -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
      before = function (entry, vim_item)
        return vim_item
      end
    })
    },
})

function goimports(wait_ms)
  local params = vim.lsp.util.make_range_params()
  params.context = {only = {"source.organizeImports"}}
  local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, wait_ms)
  for _, res in pairs(result or {}) do
    for _, r in pairs(res.result or {}) do
      if r.edit then
        vim.lsp.util.apply_workspace_edit(r.edit, "UTF-8")
      else
        vim.lsp.buf.execute_command(r.command)
      end
    end
  end
  vim.lsp.buf.formatting_sync()
end
vim.api.nvim_exec([[ autocmd BufWritePre *.go :silent! lua goimports(1000) ]], false)


vim.cmd[[highlight CmpItemAbbrMatch guibg=NONE guifg=Grey ]]
vim.cmd[[imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>']]
vim.cmd[[smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>']]
vim.cmd[[imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>']]
vim.cmd[[smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>']]


require("toggleterm").setup{
    open_mapping = [[<c-t>]],
}

local Terminal  = require('toggleterm.terminal').Terminal


function lazygit_toggle()
  local lazygit = Terminal:new({
    cmd = "lazygit" ,
    dir =  "git_dir",
    direction = "float",
    -- function to run on opening the terminal
    on_open = function(term)
      vim.cmd("startinsert!")
      vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", {noremap = true, silent = true})
    end,
  })
  lazygit:toggle()
end
vim.api.nvim_set_keymap("n", "<Leader>g", "<cmd>lua lazygit_toggle()<CR>", {noremap = true, silent = true})
vim.cmd[[
highlight GitGutterAdd    guifg=#009900 ctermfg=Grey
highlight GitGutterChange guifg=#bbbb00 ctermfg=Grey
highlight GitGutterDelete guifg=#ff2222 ctermfg=Grey
]]
vim.cmd[[hi CursorLineNr guifg=#BE2356 guibg=NONE]]

require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
        "rg",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
       "--hidden",
        "--glob=!.git/",
        "--max-filesize=10M",
        "-L",
        "-P"
    },
    preview_cutoff = 120,
  },
  pickers = {
  },
  extensions = {
      fzf = {
          fuzzy = true,
          override_generic_sorter = true,  -- override the generic sorter
          override_file_sorter = true,     -- override the file sorter
          case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
      }
  }
}

require('telescope').load_extension('fzf')

vim.cmd("let g:rooter_patterns = ['.git', 'go.mod']")
require'plenary.filetype'.add_file('tmpl')
local parser = require"nvim-treesitter.parsers".filetype_to_parsername
parser.template = "html" -- the someft filetype will use the python parser and queries.
require('go').setup()
