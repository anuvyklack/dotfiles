nmap <BS> :
xmap <BS> :

" Yank
nnoremap yc			"+y
nnoremap ycc		"+yy
nnoremap <C-c>		"+y
xnoremap <C-c>		"+y
nnoremap <C-c>c		"+y$
nnoremap <C-c><C-c>	"+yy

" " Paste
" nnoremap cp		"+p
" nnoremap cP		"+P
" xnoremap <C-p>	"+p

" Folds
nmap zr zR
nmap zm zM

" Fix writing :W to save
command! W	w
command! Q	q
command! Qa qa

" In insert mode, move normally by using Ctrl
inoremap <C-h> <Left>
inoremap <C-l> <Right>

" In command mode, move normally by using Ctrl
" cnoremap <C-h> <BS>
cnoremap <C-h> <Left>
cnoremap <C-j> <Down>
cnoremap <C-k> <Up>
cnoremap <C-l> <Right>

" " WARNING: Replaced with 'christoomey/vim-tmux-navigator' plugin
" " Quick jumping between splits
" map <C-J> <C-W>j
" map <C-K> <C-W>k
" map <C-H> <C-W>h
" map <C-L> <C-W>l

" vim: fdm=marker cc=+1 ts=4 sts=4
