vim.o.swapfile = false
vim.o.smartcase = true
vim.o.relativenumber = true
vim.o.number = true
vim.o.undofile = true
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.signcolumn = "yes"
vim.o.backupcopy= "yes"
vim.o.background = "dark"
vim.o.autoread = true
vim.o.termguicolors = true
vim.o.ignorecase = true  -- Case insensitive search
vim.o.smartcase = true -- ... but case sensitive when uc present
vim.o.wrap = false -- Do not wrap long lines
vim.o.autowrite = true -- Automatically write a file when leaving a modified buffer
vim.o.hidden = true -- Allow buffer switching without saving
vim.o.completeopt = 'menu,menuone,noselect'
vim.o.updatetime = 100
vim.o.cursorline = true
vim.o.cursorlineopt = 'number'

vim.cmd[[colorscheme gruvbox]]