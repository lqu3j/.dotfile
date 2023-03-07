return require('packer').startup(function()
	use 'wbthomason/packer.nvim'
    use 'neovim/nvim-lspconfig'
    use {'nvim-treesitter/nvim-treesitter', run = function() require('nvim-treesitter.install').update({ with_sync = true }) end}
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
	use { 'nvim-telescope/telescope.nvim', tag = '0.1.0', requires = 'nvim-lua/plenary.nvim' }
    use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
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
    -- install without yarn or npm
    use({
        "iamcco/markdown-preview.nvim",
        run = function() vim.fn["mkdp#util#install"]() end,
    })

    use 'airblade/vim-rooter'
    use 'editorconfig/editorconfig-vim'
    use 'leafOfTree/vim-vue-plugin'
    use 'crispgm/nvim-go'
    -- use 'nvim-orgmode/orgmode'
    use 'altercation/vim-colors-solarized'

    use { 'phaazon/hop.nvim', branch = 'v2'}

    use "EdenEast/nightfox.nvim"
    use "overcache/NeoSolarized"
end)
