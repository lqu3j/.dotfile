call plug#begin('~/.vim/plugged')
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'jiangmiao/auto-pairs'
Plug 'airblade/vim-gitgutter'
Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'w0rp/ale'
call plug#end()


if filereadable(expand("$HOME/.config/nvim/setting.vim"))
	source $HOME/.config/nvim/setting.vim
endif

if filereadable(expand("$HOME/.config/nvim/mapping.vim"))
	source $HOME/.config/nvim/mapping.vim
endif

" vim-go
let g:go_fmt_command = "goimports"
let g:go_autodetect_gopath = 1
let g:go_list_type = "quickfix"
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_generate_tags = 1
let g:go_def_mapping_enabled = 0
let g:go_template_autocreate = 0
let g:go_auto_type_info = 0
let g:go_updatetime = 100
" CtrlP setting
let g:ctrlp_working_path_mode = 'ra'

" Tagbar
let g:tagbar_sort = 0
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|mod|sum)$',
  \ }

" Gitgutter 
let g:gitgutter_enabled = 1

" airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = "solarized"

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#max_list = 15
call deoplete#custom#option('omni_patterns', { 'go': '[^. *\t]\.\w*' })

" ale
let g:ale_linters = {
\   'go': ['gopls'],
\}
let g:ale_sign_column_always = 0
let g:ale_linters_explicit = 1
let g:airline#extensions#ale#enabled = 1
