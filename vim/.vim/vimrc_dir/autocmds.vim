" custom autocommands and filetype settings

if has('autocmd')
  function! s:RestoreCursor() abort
    if &buftype !=# ''
      return
    endif

    let l:position = getpos("'\"")
    if l:position[1] <= 1 || l:position[1] > line('$')
      return
    endif

    call setpos('.', l:position)
    let b:dotfiles_open_fold = 1
  endfunction

  function! s:OpenRestoredFold() abort
    if !get(b:, 'dotfiles_open_fold', 0)
      return
    endif

    normal! zv
    unlet b:dotfiles_open_fold
  endfunction

  augroup dotfiles_filetypes
    autocmd!
    autocmd FileType org setlocal conceallevel=2 concealcursor=nc wrap spell
    autocmd FileType help setlocal nonumber
    autocmd FileType help nnoremap <buffer><CR> <C-]>
    autocmd FileType help nnoremap <buffer><BS> <C-T>
  augroup END

  augroup dotfiles_restore_cursor
    autocmd!
    autocmd BufReadPost * call <SID>RestoreCursor()
    " Open folds only after modelines have been processed.
    autocmd BufWinEnter * call <SID>OpenRestoredFold()
  augroup END
endif
