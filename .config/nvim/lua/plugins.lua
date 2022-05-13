return require('packer').startup(function()
	use 'wbthomason/packer.nvim'
    use 'neovim/nvim-lspconfig'
    use {'nvim-treesitter/nvim-treesitter', commit='aadad4738e8453b3b0928ba0aa8f49b6878abace'}
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-vsnip'
    use 'hrsh7th/vim-vsnip'
    use 'kshenoy/vim-signature'
    use 'f-person/git-blame.nvim'
    use 'tpope/vim-surround'
	use { 'kyazdani42/nvim-tree.lua', requires = 'kyazdani42/nvim-web-devicons' }
	use { 'nvim-telescope/telescope.nvim', requires = 'nvim-lua/plenary.nvim' }
    use 'nvim-telescope/telescope-project.nvim'
    use { 'hoob3rt/lualine.nvim', requires = 'kyazdani42/nvim-web-devicons' }
    use 'windwp/nvim-autopairs'
    use 'mhinz/vim-startify'
    use 'sainnhe/gruvbox-material'
    use 'terryma/vim-multiple-cursors'
    use 'sickill/vim-monokai'
    use 'ayu-theme/ayu-vim'
    use 'ishan9299/nvim-solarized-lua'
    use 'numToStr/Comment.nvim'
    use 'sonph/onehalf'
    use 'morhetz/gruvbox'
    use 'onsails/lspkind-nvim'
    use 'akinsho/toggleterm.nvim'
    use 'airblade/vim-gitgutter'
    use {'iamcco/markdown-preview.nvim', run = 'cd app && yarn install'}
end)