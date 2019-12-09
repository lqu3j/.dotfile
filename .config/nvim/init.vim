call plug#begin('~/.vim/plugged')

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'jiangmiao/auto-pairs'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'Shougo/echodoc.vim'
Plug 'w0rp/ale'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'easymotion/vim-easymotion'
Plug 'terryma/vim-multiple-cursors'

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

set cmdheight=2

" Use space instead of tabs
set expandtab
set shiftwidth=4
set tabstop=4

" Show line number
set number

" Highlight cursor line
set cursorline
let g:gruvbox_contrast_dark='medium'
set background=dark
let g:gruvbox_invert_signs=0
let g:gruvbox_sign_column='bg0'
colorscheme gruvbox

set shortmess+=c
set completeopt=menu,noinsert,noselect

" vim-go
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_generate_tags = 1
let g:go_auto_type_info = 1
let g:go_fmt_command = "goimports"


set autoread                    " Automatically read changed files
set noshowmatch                 " Do not show matching brackets by flickering
set noshowmode                  " We show the mode with airline or lightline

let g:airline_powerline_fonts = 0
let g:airline_theme = "gruvbox"


nnoremap <leader>w :w<Cr>

let g:deoplete#enable_at_startup = 1

set completeopt=longest,menuone
set signcolumn=yes

let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

let g:ale_linters = {
	\ 'go': ['gopls'],
	\}

call deoplete#custom#option('omni_patterns', {
\ 'go': '[^. *\t]\.\w*',
\})

nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)

if has('persistent_undo')
  set undofile
  set undodir=~/.config/vim/tmp/undo//
endif


" FZF config
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

nmap <Leader>bb :Buffers<CR>
" File, recent file
nmap <Leader>fr :History<CR>
" File, project file
nmap <Leader>fp :GFiles<CR>
" File, open file
nmap <Leader>ff :Files<CR>
nmap <Leader>/ :Ag<Space>

nmap <M-?> :GoReferrers<CR>
nmap <M-x> :Commands<CR>
nmap <Leader>q :q<CR>

set hidden
set splitright

nmap s <Plug>(easymotion-overwin-f2)
let g:EasyMotion_smartcase = 1

set noswapfile
autocmd FileType help,fugitive wincmd L
