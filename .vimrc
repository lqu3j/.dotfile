" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-repeat'
Plug 'scrooloose/nerdcommenter'
Plug 'altercation/vim-colors-solarized'
" Initialize plugin system
call plug#end()

set nu
set autowrite

" ctrlp setting
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_user_command = 'find %s -type f'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
nnoremap <C-x><C-r> :CtrlPMRU<CR>
nnoremap <C-x>b :CtrlPBuffer<CR>

" vim-go setting
let g:go_fmt_command = "goimports"
let g:go_def_mode = 'godef'
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_function_calls = 1
let g:go_auto_type_info = 1
let g:go_list_type = "quickfix"
let g:go_metalinter_disabled = ['golint']
let g:go_metalinter_autosave = 1

let g:go_gocode_propose_builtins = 1
let g:go_gocode_unimported_packages = 1
autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4
nnoremap <C-j> :cnext<CR>
nnoremap <C-k> :cprevious<CR>

" airline setting
let g:airline_theme='solarized'
let g:airline_solarized_bg='dark'
let g:airline_powerline_fonts = 1

" monokai
syntax enable
set background=dark
colorscheme solarized

" nerdcommenter
let g:NERDSpaceDelims = 1

" json format
command! JsonFormat :execute '%!python -m json.tool'

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent
