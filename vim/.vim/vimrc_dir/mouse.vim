" mouse settings

" Enable mouse support in console
if has('mouse')
  set mouse=a
  if !has('nvim')
    if ($TERM =~ "xterm" || $TERM =~ "screen" || $TERM =~ "urxvt" || $TERM =~ "ghostty")
      " for some reason, doing this directly with 'set ttymouse=xterm2'
      " doesn't work -- 'set ttymouse?' returns xterm2 but the mouse
      " makes tmux enter copy mode instead of selecting or scrolling
      " inside Vim -- but luckily, setting it up from within autocmds
      " works
      augroup vim_ttymouse
        autocmd!
        autocmd VimEnter,FocusGained,BufEnter * set ttymouse=xterm2
      augroup END
    endif
  endif
endif
