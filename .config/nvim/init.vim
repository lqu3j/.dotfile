" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin()

" Make sure you use single quotes
" Plug 'valloric/youcompleteme'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" Plug 'bling/vim-airline'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'kien/ctrlp.vim'
" Plug 'vim-airline/vim-airline-themes'
Plug 'majutsushi/tagbar'
Plug 'justinmk/vim-syntax-extra'
Plug 'tomasr/molokai'
Plug 'joshdick/onedark.vim'
Plug 'SirVer/ultisnips'
Plug 'scrooloose/syntastic'
Plug 'jiangmiao/auto-pairs'
Plug 'sheerun/vim-polyglot'

" Initialize plugin system
call plug#end()



" VIM perferance setting
set cursorline
" set relativenumber
set nu
set history=200
syntax on
"colorscheme onedark
" let g:airline_theme='solarized'
" let g:airline_solarized_bg='dark'

" Vim better default setting
let mapleader = "\<Space>"
set wildignore+=*.so,*.swp,*.zip

" Plugin setting
" airline setting
" let g:airline#extensions#tabline#enabled = 1
" let g:airline_powerline_fonts = 1

" vim-go setting
let g:go_def_mode = 'godef'
let g:go_highlight_fields = 1
let g:go_highlight_function_calls = 1
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1

" Go source file setting
au FileType go set noexpandtab
au Filetype go set shiftwidth=4
au Filetype go set softtabstop=4
au Filetype go set tabstop=4

" CtrlP setting
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode='ra'
let g:ctrlp_custom_ignore='\v[\/]\.(git|hg|svn)$'

" NERDTree setting
" How can I close vim if the only window left open is a NERDTree?
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif


" YCM setting
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_key_invoke_completion = '<C-Space>'
let g:ycm_python_binary_path = '/usr/bin/python'

" UltiSnips setting
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" syntastic setting
let g:syntastic_go_checkers = ['go', 'govet']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1 
let g:syntastic_enable_signs = 0
set statusline+=%#warningmsg#                                                                                                                                                              
set statusline+=%{SyntasticStatuslineFlag()}                                                                                                                                               
set statusline+=%*                                                                                                                                                                         

" Keybindings setting
map <leader>tn :NERDTreeToggle<CR>
map <leader>tt :Tagbar<CR>
map <leader>bb :CtrlPBuffer<CR>
map <leader>fr :CtrlPMRUFiles<CR>

" Tagbar setting
let g:tagbar_sort=0

