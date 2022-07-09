" Move to the beginning / end of a line with "Shift + h/l"
nnoremap H ^
nnoremap L $
xnoremap H ^
xnoremap L $
onoremap H ^
onoremap L $

nnoremap <BS> :
xnoremap <BS> :

" Yank
nnoremap yc		"+y
nnoremap ycc	"+yy
nnoremap <C-c>	"+y
xnoremap <C-c>	"+y
nnoremap <C-c><C-c>  "+yy

" Paste
nnoremap cp		"+p
nnoremap cP		"+P
xnoremap <C-p>	"+p

" Fix writing :W to save
command! W	w
command! Q	q
command! Qa qa

" In insert mode, move normally by using Ctrl
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
