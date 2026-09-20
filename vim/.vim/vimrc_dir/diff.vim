" vim diff settings

" Use the internal histogram diff algorithm with the indent heuristic.
" ref: https://github.com/vim/vim/pull/2732
" and https://github.com/neovim/neovim/issues/1466
set diffopt+=vertical,internal,algorithm:histogram,indent-heuristic
