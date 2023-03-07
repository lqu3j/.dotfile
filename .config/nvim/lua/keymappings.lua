vim.g.mapleader = ' '

-- no hl
vim.api.nvim_set_keymap('n', '<Leader>h', ':set hlsearch!<CR>', { noremap = true, silent = true })

-- explorer
vim.api.nvim_set_keymap('n', '<Leader>e', ':NvimTreeOpen %:h<CR>', { noremap = true, silent = true })

-- better window movement
vim.api.nvim_set_keymap('n', '<A-h>', '<C-w>h', { silent = true })
vim.api.nvim_set_keymap('n', '<A-j>', '<C-w>j', { silent = true })
vim.api.nvim_set_keymap('n', '<A-k>', '<C-w>k', { silent = true })
vim.api.nvim_set_keymap('n', '<A-l>', '<C-w>l', { silent = true })

-- better indenting
vim.api.nvim_set_keymap('v', '<', '<gv', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '>', '>gv', { noremap = true, silent = true })

-- I hate escape
vim.api.nvim_set_keymap('i', 'jk', '<ESC>', { noremap = true, silent = true })

-- Move selected line / block of text in visual mode
vim.api.nvim_set_keymap('x', 'J', ":move '>+1<CR>gv-gv", { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', 'K', ":move '<-2<CR>gv-gv", { noremap = true, silent = true })


vim.api.nvim_set_keymap('n', ']q', ':cnext<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '[q', ':cprevious<CR>', { noremap = true, silent = true })


vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '*', '*N', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '#', '#n', { noremap = true, silent = true })


vim.api.nvim_set_keymap('n', '<C-l>', ':<C-u>nohlsearch<CR><C-l>',{ noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>q', ':q<CR>', { noremap = true, silent = true })
