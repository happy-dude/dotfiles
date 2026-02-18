" Use Ctrl-e to close pop-up menu; preserve vim-rsi readline behavior and popup close
inoremap <silent><expr> <C-e>
      \ coc#pum#visible() ? coc#pum#cancel() :
      \ col('.')>strlen(getline('.')) ? "\<C-e>" : "\<End>"

