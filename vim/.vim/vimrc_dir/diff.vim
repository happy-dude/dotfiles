" vim diff settings

" Use xdiff patience algorithm
" ref: https://github.com/vim/vim/pull/2732
" and https://github.com/neovim/neovim/issues/1466
set diffopt+=vertical,internal,algorithm:histogram,indent-heuristic
