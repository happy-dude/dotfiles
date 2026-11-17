" vim-rsi and NERDCommenter map these keys from their plugin scripts, which
" run after the vimrc, so CoC's mappings must load from after/ to win.
if !exists('g:did_coc_loaded')
  finish
endif

" Close the CoC popup menu; otherwise keep vim-rsi's behaviour, which cancels
" a native completion menu and otherwise moves to the end of the line.
inoremap <silent><expr> <C-e>
      \ coc#pum#visible() ? coc#pum#cancel() :
      \ col('.')>strlen(getline('.')) <Bar><Bar> pumvisible() ? "\<C-e>" : "\<End>"

" Run the Code Lens action on the current line, in place of NERDCommenter's
" AlignLeft comment.
nmap <leader>cl  <Plug>(coc-codelens-action)
