set nomore
execute 'source ' . fnameescape($DOTFILES_CACHE_VIM)

execute 'edit ' . fnameescape($HOME . '/ordinary.txt')
call assert_true(&l:backup)
call assert_true(&l:writebackup)
call assert_true(&l:swapfile)
call assert_true(&l:undofile)

execute 'edit ' . fnameescape($HOME . '/.config/rclone/rclone.conf')
call assert_false(&l:backup)
call assert_false(&l:writebackup)
call assert_false(&l:swapfile)
call assert_false(&l:undofile)

execute 'edit ' . fnameescape($HOME . '/.config/nix/local.conf')
call assert_false(&l:backup)
call assert_false(&l:writebackup)
call assert_false(&l:swapfile)
call assert_false(&l:undofile)

execute 'edit ' . fnameescape($HOME . '/.config/opencode/local.json')
call assert_false(&l:backup)
call assert_false(&l:writebackup)
call assert_false(&l:swapfile)
call assert_false(&l:undofile)

if !empty(v:errors)
  for error in v:errors
    echomsg error
  endfor
  cquit
endif
quitall!
