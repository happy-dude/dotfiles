" vim-rsi maps <C-e> from its plugin script, which runs after the vimrc, so
" this mapping must load from after/ to win.
if !exists('g:did_coc_loaded')
  finish
endif

" Close the CoC popup menu; otherwise keep vim-rsi's end-of-line behaviour.
inoremap <silent><expr> <C-e>
      \ coc#pum#visible() ? coc#pum#cancel() :
      \ col('.')>strlen(getline('.')) ? "\<C-e>" : "\<End>"
