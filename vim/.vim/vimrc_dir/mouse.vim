" mouse settings

" Enable mouse support in console
if has('mouse')
  set mouse=a
  if !has('nvim')
    if ($TERM =~ "xterm" || $TERM =~ "screen" || $TERM =~ "tmux" || $TERM =~ "urxvt" || $TERM =~ "ghostty")
      " for some reason, doing this directly with 'set ttymouse=xterm2'
      " doesn't work -- 'set ttymouse?' returns xterm2 but the mouse
      " makes tmux enter copy mode instead of selecting or scrolling
      " inside Vim -- but luckily, setting it up from within autocmds
      " works
      " xterm2 stops at column 223; SGR has no limit and every terminal
      " listed above speaks it.
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
