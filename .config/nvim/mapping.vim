" Toggle Relative Number
nnoremap <silent><leader>nr :set relativenumber!<CR>
" Keep search results at the center of screen
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
nnoremap Y y$
noremap <silent> <leader><cr> :noh<cr>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
noremap <leader>tN :tabnew<CR>
noremap <leader>tc :tabclose<CR>
noremap <leader>tm :tabmove<CR>
noremap <leader>tn :tabnext<CR>
noremap <leader>tp :tabprevious<CR>
nnoremap <leader>r :source $MYVIMRC<CR>


nnoremap <leader>fr :CtrlPMRUFiles<CR>
nnoremap <leader>bb :CtrlPBuffer<CR>
nnoremap <leader>bd :bd<CR>
nnoremap <leader>ff :CtrlP<CR>
nnoremap <leader>w :w<Cr>
nnoremap <leader>q :q!<Cr>
nnoremap <leader>nt :NERDTreeToggle<Cr>
nnoremap <leader>tt :Tagbar<CR>

map <C-]> :GoDef<Cr>
