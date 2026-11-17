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

" Scroll a scrollable CoC float; otherwise keep vim-rsi's behaviour. <C-f>
" moves right, or sends <C-f> (reindent with !^F in 'indentkeys') at the end
" of the line. <C-b> moves left, or joins a blank line onto the one above
" when the cursor is at its end. The expressions are copied from vim-rsi's
" plugin/rsi.vim, because this mapping replaces its own.
"
" coc#float#has_scroll() also counts CoC's completion menu, which
" coc#float#scroll() skips, so check for the windows it scrolls; otherwise a
" long menu alone would make both keys do nothing.
function! s:CanScrollFloat() abort
  return !empty(filter(coc#float#get_float_win_list(),
        \ 'coc#float#scrollable(v:val) && getwinvar(v:val, "kind", "") !=# "pum"'))
endfunction
inoremap <silent><nowait><expr> <C-f>
      \ <SID>CanScrollFloat() ? "\<C-r>=coc#float#scroll(1)\<CR>" :
      \ col('.')>strlen(getline('.')) ? "\<C-f>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b>
      \ <SID>CanScrollFloat() ? "\<C-r>=coc#float#scroll(0)\<CR>" :
      \ getline('.')=~'^\s*$' && col('.')>strlen(getline('.')) ? "0\<C-d>\<Esc>kJs" : "\<Left>"

" Run the Code Lens action on the current line, in place of NERDCommenter's
" AlignLeft comment.
nmap <leader>cl  <Plug>(coc-codelens-action)
