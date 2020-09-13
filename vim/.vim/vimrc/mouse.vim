" mouse settings

" Enable mouse support in console
if has('mouse')
    set mouse=a
    if !has('nvim')
        if ($TERM =~ "xterm" || $TERM =~ "screen" || $TERM =~ "urxvt")
            " for some reason, doing this directly with 'set ttymouse=xterm2'
            " doesn't work -- 'set ttymouse?' returns xterm2 but the mouse
            " makes tmux enter copy mode instead of selecting or scrolling
            " inside Vim -- but luckily, setting it up from within autocmds
            " works
            autocmd VimEnter * set ttymouse=xterm2
            autocmd FocusGained * set ttymouse=xterm2
            autocmd BufEnter * set ttymouse=xterm2
        endif
    endif
endif
