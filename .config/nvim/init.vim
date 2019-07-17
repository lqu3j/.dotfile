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
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'easymotion/vim-easymotion'
Plug 'yggdroot/indentline'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'plasticboy/vim-markdown'
Plug 'suan/vim-instant-markdown', {'for': 'markdown'}
Plug 'luochen1990/rainbow'
call plug#end()

" The leader key
let mapleader = ","

" Convenience way to save file as sudo
command! W w !sudo tee % > /dev/null

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" This enables us to undo files even if you exit Vim.
if has('persistent_undo')
  set undofile
  set undodir=~/.config/vim/tmp/undo//
endif

" Setting background, to adjust the default color scheme
set background=dark

" Using theme 
colorscheme gruvbox

" Show the line number relative to the line 
set relativenumber

" Use space instead of tabs
set expandtab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" Do not jump to forward
nnoremap * *``

nnoremap <leader>fr :CtrlPMRUFiles<CR>
nnoremap <leader>bb :CtrlPBuffer<CR>
nnoremap <leader>ff :CtrlP<CR>
nnoremap <leader>w :w<Cr>
nnoremap <leader>q :q!<Cr>
nnoremap <leader>d :GoDeclsDir<CR>
autocmd FileType go nnoremap <buffer> <C-]> :GoDef<CR>

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
let g:airline_theme = "gruvbox"

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#max_list = 15

" ale
let g:ale_linters = {
\   'go': ['gopls'],
\}
let g:ale_linters_explicit = 1
let g:airline#extensions#ale#enabled = 1

" nerdcommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" indentline
let g:indentLine_enabled = 1

set cursorline
" hi clear CursorLine
" hi CursorLine gui=underline cterm=underline ctermfg=None guifg=None

" s{char}{char} to move to {char}{char}
" nmap f <Plug>(easymotion-overwin-f)
" nmap s <Plug>(easymotion-overwin-f2)
" nmap w <Plug>(easymotion-overwin-w)
" <Leader>f{char} to move to {char}
nmap <C-x>; <Plug>(easymotion-bd-f)

autocmd FileType tagbar,nerdtree setlocal signcolumn=no

set diffopt=vertical,context:50

autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
set conceallevel=0

let g:ctrlp_working_path_mode = 'rc'
let g:ctrlp_show_hidden = 1
let g:go_def_reuse_buffer = 1

augroup gopkgs
  autocmd!
  autocmd FileType go command! -buffer Import  call fzf#run({'source': 'gopkgs -no-vendor', 'sink': 'GoImport', 'down': '30%'})
augroup END

"Uncomment to override defaults:
let g:instant_markdown_slow = 1
let g:instant_markdown_autostart = 0
let g:instant_markdown_open_to_the_world = 1 
let g:instant_markdown_allow_unsafe_content = 1
let g:instant_markdown_allow_external_content = 0
let g:instant_markdown_mathjax = 1

let g:rainbow_active = 1
