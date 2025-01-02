require("plugins")
require("keymappings")
require("settings")

vim.o.completeopt = "menuone,noinsert,noselect"

require("nvim-tree").setup({
  sync_root_with_cwd = true,
  respect_buf_cwd = true,
  update_focused_file = {
    enable = true,
    update_root = true
  },
}
)

local treesitter = require("nvim-treesitter")

local function treelocation()
	return treesitter.statusline({
		indicator_size = 70,
		type_patterns = { "class", "function", "method" },
		separator = " -> ",
	})
end

require("nvim-treesitter.configs").setup({
	ensure_installed = { "lua", "go", "javascript", "json", "html", "pug" },
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
})

vim.cmd([[autocmd VimEnter * highlight clear SignColumn]])
vim.cmd([[command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!]])

vim.api.nvim_set_keymap(
	"n",
	"<Leader>ff",
	[[<Cmd>lua require('telescope.builtin').find_files()<CR>]],
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<Leader>fp",
	[[<Cmd>lua require('telescope.builtin').git_files()<CR>]],
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<Leader>fr",
	[[<Cmd>lua require('telescope.builtin').oldfiles()<CR>]],
	{ noremap = true, silent = true }
)
-- vim.api.nvim_set_keymap(
-- 	"n",
-- 	"<Leader>fg",
-- 	[[<Cmd>lua require('telescope.builtin').live_grep()<CR>]],
-- 	{ noremap = true, silent = true }
-- )
vim.api.nvim_set_keymap(
	"n",
	"<Leader>fg",
	[[<Cmd>lua require('telescope.builtin').live_grep()<CR>]],
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<Leader>fb",
	[[<Cmd>lua require('telescope.builtin').buffers()<CR>]],
	{ noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
	"n",
	"<C-p>",
	[[<Cmd>lua require('telescope').extensions.project.project({display_type = 'full'})<CR>]],
	{ noremap = true, silent = true }
)

local lsp = require("lspconfig")
local util = require("lspconfig/util")

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
	-- passing in a table with on_attach function
	local function buf_set_keymap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end
	local function buf_set_option(...)
		vim.api.nvim_buf_set_option(bufnr, ...)
	end
	-- Mappings.
	local opts = { noremap = true, silent = true }

	buf_set_keymap("n", "<Leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
	buf_set_keymap(
		"n",
		"<Leader>fs",
		[[<Cmd>lua require('telescope.builtin').lsp_dynamic_workspace_symbols()<CR>]],
		opts
	)
	buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
	buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
	buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
	buf_set_keymap("n", "<Leader>xx", "<cmd>lua vim.diagnostic.setloclist()<cr>", opts)
end

-- Setup lspconfig.
local capabilities = require("cmp_nvim_lsp").default_capabilities()

require("go").setup({
    auto_lint = false,
    lint_prompt_style = "vt"
})
lsp.gopls.setup({
	on_attach = on_attach,
	capabilities = capabilities,
	init_options = {
		usePlaceholders = true,
	},
	root_dir = lsp.util.root_pattern(".git"),
	settings = {
		-- gopls = {
		--     analyses = {
		--         unusedparams = true,
		--     },
		-- }
	},
})

-- require("lspconfig").tsserver.setup({
-- 	on_attach = on_attach,
-- 	capabilities = capabilities,
-- })

-- require'lspconfig'.vuels.setup{
--     on_attach = on_attach,
--     capabilities = capabilities,
-- }
--
require("lspconfig").eslint.setup({})

require("nvim-autopairs").setup({
	disable_filetype = { "TelescopePrompt", "vim" },
})

vim.o.lazyredraw = true

require("lualine").setup({
	options = { theme = "gruvbox" },
	sections = {
		lualine_b = {
			{ "filename", path = 1 },
			{ "diagnostics" },
		},
		lualine_c = { { treelocation } },
		lualine_x = { "encoding", "branch", "filetype" },
	},
})
require("Comment").setup()

local cmp = require("cmp")
local lspkind = require("lspkind")
cmp.setup({
	preselect = cmp.PreselectMode.None,
	snippet = {
		expand = function(args)
			vim.fn["vsnip#anonymous"](args.body)
		end,
	},
	mapping = {
		["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
		["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
		["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
		["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
		["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
		["<C-e>"] = cmp.mapping({
			i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
		["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
	},
	sources = cmp.config.sources({
		{ name = "nvim_lsp", trigger_characters = { "." } },
		{ name = "vsnip" },
	}, {
		{ name = "buffer" },
		{ name = "path" },
	}),
	formatting = {
		format = function(entry, vim_item)
			if vim.tbl_contains({ "path" }, entry.source.name) then
				local icon, hl_group = require("nvim-web-devicons").get_icon(entry:get_completion_item().label)
				if icon then
					vim_item.kind = icon
					vim_item.kind_hl_group = hl_group
					return vim_item
				end
			end
			return require("lspkind").cmp_format({ with_text = true })(entry, vim_item)
		end,
	},
})

function goimports(wait_ms)
	local params = vim.lsp.util.make_range_params()
	params.context = { only = { "source.organizeImports" } }
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
	vim.lsp.buf.format({async = false})
end
vim.api.nvim_exec([[ autocmd BufWritePre *.go :silent! lua goimports(1000) ]], false)

vim.cmd([[highlight CmpItemAbbrMatch guibg=NONE guifg=Grey ]])
vim.cmd([[imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>']])
vim.cmd([[smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>']])
vim.cmd([[imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>']])
vim.cmd([[smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>']])

require("toggleterm").setup({
	open_mapping = [[<c-t>]],
})

local Terminal = require("toggleterm.terminal").Terminal

function lazygit_toggle()
	local lazygit = Terminal:new({
		cmd = "lazygit",
		dir = "git_dir",
		direction = "float",
		-- function to run on opening the terminal
		on_open = function(term)
			vim.cmd("startinsert!")
			vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", { noremap = true, silent = true })
		end,
	})
	lazygit:toggle()
end
vim.api.nvim_set_keymap("n", "<Leader>g", "<cmd>lua lazygit_toggle()<CR>", { noremap = true, silent = true })
vim.cmd([[
highlight GitGutterAdd            guifg=#009900 ctermfg=Grey
highlight GitGutterChange         guifg=#bbbb00 ctermfg=Grey
highlight GitGutterDelete         guifg=#ff2222 ctermfg=Grey
highlight GitGutterChangeDelete   guifg=#8ec07c ctermfg=Grey
]])
vim.cmd([[hi CursorLineNr guifg=#BE2356 guibg=NONE]])

require("telescope").setup({
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
			"-P",
		},
		preview = {
			treesitter = false,
			hide_on_startup = true,
		},
	},
	pickers = {
		find_files = {
			find_command = { "fd", "--type", "f", "--hidden", "--follow", "--exclude",".git"},
		},
	},
	extensions = {
		fzf = {
			fuzzy = true,
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true, -- override the file sorter
			case_mode = "smart_case", -- or "ignore_case" or "respect_case"
		},
		project = {
            theme = "ivy"
		},
	},
})

require("telescope").load_extension("project")
require("telescope").load_extension("fzf")

vim.cmd("let g:rooter_patterns = ['.git', 'go.mod']")
require("plenary.filetype").add_file("tmpl")

vim.cmd([[
    autocmd FileType go compiler go
    autocmd QuickFixCmdPost [^l]* nested cwindow
]])


require("hop").setup({ keys = "etovxqpdygfblzhckisuran" })
-- place this in one of your configuration file(s)
vim.api.nvim_set_keymap(
	"n",
	"f",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"F",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"t",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })<cr>",
	{}
)
vim.api.nvim_set_keymap(
	"n",
	"T",
	"<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })<cr>",
	{}
)

vim.cmd[[cnoremap <expr> %% getcmdtype( ) == ':' ? expand('%:h').'/' : '%%']]
-- vim.cmd[[nnoremap mk :silent make %<cr>]]
function _G.set_terminal_keymaps()
  local opts = {buffer = 0}
  vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
  vim.keymap.set('t', '<alt-h>', [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set('t', '<alt-j>', [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set('t', '<alt-k>', [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set('t', '<alt-l>', [[<Cmd>wincmd l<CR>]], opts)
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')
vim.cmd [[
  autocmd FileType qf nnoremap <buffer> q :cclose<CR>
]]

