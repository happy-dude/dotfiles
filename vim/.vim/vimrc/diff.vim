" vim diff settings

" Use xdiff patience algorithm
" ref: https://github.com/vim/vim/pull/2732
" and https://github.com/neovim/neovim/issues/1466
if has("patch-8.1.0360") || has('nvim')
    set diffopt+=vertical,internal,algorithm:patience
endif
