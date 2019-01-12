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
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-repeat'
Plug 'scrooloose/nerdcommenter'
Plug 'rking/ag.vim'
Plug 'mdempsky/gocode', { 'rtp': 'vim', 'do': '~/.vim/plugged/gocode/vim/symlink.sh' }
" Initialize plugin system
call plug#end()

set nonu
set autowrite
let mapleader = ","

" ctrlp setting
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_user_command = 'find %s -type f'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_map = '<C-x><C-f>'
let g:ctrlp_cmd = 'CtrlP'
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
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
let g:go_metalinter_autosave = 1
let g:go_gocode_propose_builtins = 1
let g:go_gocode_unimported_packages = 0
autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4
nnoremap <C-j> :cnext<CR>
nnoremap <C-k> :cprevious<CR>

" airline setting
let g:airline_theme='angr'
let g:airline_powerline_fonts = 1

" monokai
" set background=dark

" nerdcommenter
let g:NERDSpaceDelims = 1

" ag
let g:ag_working_path_mode="r"
let g:ag_highlight=1
let g:ag_prg="ag --vimgrep --smart-case"
let g:ag_mapping_message=0
nnoremap <C-x><C-s> :Ag

" json format
command! JsonFormat :execute '%!python -m json.tool'

" YCM
let g:ycm_autoclose_preview_window_after_insertion = 1
