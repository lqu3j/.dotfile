call plug#begin('~/.vim/plugged')
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'jiangmiao/auto-pairs'
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'easymotion/vim-easymotion'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'plasticboy/vim-markdown'
Plug 'suan/vim-instant-markdown', {'for': 'markdown'}
Plug 'luochen1990/rainbow'
Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
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
nnoremap <leader>q :bd<Cr>
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
let g:go_auto_type_info = 1
let g:go_def_reuse_buffer = 1
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

augroup gopkgs
  autocmd!
  autocmd FileType go command! -buffer Import  call fzf#run({'source': 'gopkgs -no-vendor', 'sink': 'GoImport'})
augroup END


" CtrlP setting
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|mod|sum)$',
  \ }
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_show_hidden = 1

" Tagbar
let g:tagbar_sort = 0

" Gitgutter 
let g:gitgutter_enabled = 1

" airline
let g:airline_powerline_fonts = 0
let g:airline_theme = "gruvbox"
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0

" nerdcommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

nmap <C-x>; <Plug>(easymotion-bd-f)

autocmd FileType tagbar,nerdtree setlocal signcolumn=no

autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler norelativenumber
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler relativenumber

autocmd TermOpen * setlocal nonumber norelativenumber signcolumn=no
let g:fzf_layout= { 'down': '30%' }

let g:rainbow_active = 1
command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, {'options': '--delimiter : --nth 4..'}, <bang>0)
command! -bang -nargs=* Rg call fzf#vim#ag(<q-args>, {'options': '--delimiter : --nth 4..'}, <bang>0)
" Automatically change the current directory
autocmd BufEnter * silent! lcd %:p:h
let g:instant_markdown_autostart = 1
let g:vim_markdown_folding_disabled = 1
" Switch buffer without saveing
set hidden
" Set to auto read when a file is changed from the outside
set autoread
" Always show current position
set ruler
" Ignore case when searching
set ignorecase
" When searching try to be smart about cases
set smartcase
" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

set switchbuf=useopen,usetab,newtab
set stal=2

" Always show the status line
set laststatus=2
" Remap VIM 0 to first non-blank character
map 0 ^
" Set 10 lines to the cursor - when moving vertically using j/k
set so=10

tnoremap <Esc> <C-\><C-n>

nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz

" Better display for messages
set cmdheight=2
" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=100
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Add status line support, for integration with other plugin, checkout `:h
" coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')
