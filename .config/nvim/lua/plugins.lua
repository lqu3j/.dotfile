return require("packer").startup(function()
	use("wbthomason/packer.nvim")
	use("neovim/nvim-lspconfig")
	use({
		"nvim-treesitter/nvim-treesitter",
		run = function()
			require("nvim-treesitter.install").update({ with_sync = true })
		end,
	})
    -- Lua
    use {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup {
          -- your configuration comes here
          -- or leave it empty to use the default settings
          -- refer to the configuration section below
        }
      end
    }

	use("hrsh7th/cmp-nvim-lsp")
	use("hrsh7th/cmp-buffer")
	use("hrsh7th/cmp-path")
	use("hrsh7th/nvim-cmp")
	use("hrsh7th/cmp-vsnip")
	use("hrsh7th/vim-vsnip")
	use("kshenoy/vim-signature")
	use("f-person/git-blame.nvim")
	use("tpope/vim-surround")
	use({ "kyazdani42/nvim-tree.lua", requires = "kyazdani42/nvim-web-devicons" })
	use({ "nvim-telescope/telescope.nvim", requires = "nvim-lua/plenary.nvim" })
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use("nvim-telescope/telescope-project.nvim")
	use({ "hoob3rt/lualine.nvim", requires = "kyazdani42/nvim-web-devicons" })
	use("windwp/nvim-autopairs")
	use("mhinz/vim-startify")
	use("terryma/vim-multiple-cursors")
	use("sickill/vim-monokai")
	use("numToStr/Comment.nvim")
	use("sonph/onehalf")
	use("morhetz/gruvbox")
	use("onsails/lspkind-nvim")
	use("akinsho/toggleterm.nvim")
	use("airblade/vim-gitgutter")
    use {'kevinhwang91/nvim-bqf'}
	-- install without yarn or npm
	use({
		"iamcco/markdown-preview.nvim",
		run = function()
			vim.fn["mkdp#util#install"]()
		end,
	})

	use("airblade/vim-rooter")
	use("editorconfig/editorconfig-vim")
	use("leafOfTree/vim-vue-plugin")
	use("crispgm/nvim-go")
	use({ "phaazon/hop.nvim", branch = "v2" })
	use({ "mhartington/formatter.nvim" })
    use("vim-autoformat/vim-autoformat")
    use{"SmiteshP/nvim-navic",requires = "neovim/nvim-lspconfig"}
    use {
    'maxmx03/solarized.nvim',
    config = function()
      vim.o.background = 'dark'
      ---@type solarized
      local solarized = require('solarized')
      vim.o.termguicolors = true
      vim.o.background = 'dark'
      solarized.setup({})
      vim.cmd.colorscheme 'solarized'
    end
}
end)
