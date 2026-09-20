" mouse settings

" Enable mouse support in console
if has('mouse')
  set mouse=a
  if !has('nvim')
    if ($TERM =~ "xterm" || $TERM =~ "screen" || $TERM =~ "tmux" || $TERM =~ "urxvt" || $TERM =~ "ghostty")
      " Reassert the mouse protocol on entry and focus changes; without this,
      " tmux can handle drags as copy-mode input instead of passing them to Vim.
      " Prefer SGR when Vim supports it; xterm2 is limited to column 223.
      augroup vim_ttymouse
        autocmd!
        if has('mouse_sgr')
          autocmd VimEnter,FocusGained,BufEnter * set ttymouse=sgr
        else
          autocmd VimEnter,FocusGained,BufEnter * set ttymouse=xterm2
        endif
      augroup END
    endif
  endif
endif
