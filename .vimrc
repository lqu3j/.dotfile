" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

Plug 'valloric/youcompleteme'
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'scrooloose/syntastic'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'altercation/vim-colors-solarized'

" Initialize plugin system
call plug#end()


" Go source file setting
au FileType go set noexpandtab
au Filetype go set shiftwidth=4
au Filetype go set softtabstop=4
au Filetype go set tabstop=4

" vim-go setting
let g:go_def_mode = 'godef'
let g:go_highlight_fields = 1
let g:go_highlight_function_calls = 1
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1

" syntastic setting
let g:syntastic_go_checkers = ['go', 'govet']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_enable_signs = 0

" airline setting
let g:airline_theme='base16_monokai'
let g:airline_powerline_fonts = 1

hi link diffRemoved DiffDelete
hi link diffAdded DiffAdd
syntax on
set nu
set background=dark
colorscheme solarized
let g:solarized_diffmode="high"
