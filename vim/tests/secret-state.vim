set nomore
" The default patterns cover the temporary directory this test runs in;
" only the entries cache.vim adds are under test.
set backupskip=
execute 'source ' . fnameescape($DOTFILES_CACHE_VIM)

" Vim skips the backup when any 'backupskip' pattern matches the buffer
" name as given, without resolving symlinks.
function! s:BackupSkipped(path) abort
  let l:file = fnamemodify(expand(a:path), ':p')
  for l:pattern in split(&backupskip, ',')
    if l:file =~# glob2regpat(l:pattern)
      return 1
    endif
  endfor
  return 0
endfunction

execute 'edit ' . fnameescape($HOME . '/ordinary.txt')
call assert_true(&l:backup)
call assert_true(&l:writebackup)
call assert_true(&l:swapfile)
call assert_true(&l:undofile)

execute 'edit ' . fnameescape($HOME . '/.config/rclone/rclone.conf')
call assert_true(s:BackupSkipped($HOME . '/.config/rclone/rclone.conf'))
call assert_false(&l:swapfile)
call assert_false(&l:undofile)

execute 'edit ' . fnameescape($HOME . '/.config/nix/local.conf')
call assert_true(s:BackupSkipped($HOME . '/.config/nix/local.conf'))
call assert_false(&l:swapfile)
call assert_false(&l:undofile)

execute 'edit ' . fnameescape($HOME . '/.config/opencode/local.json')
call assert_true(s:BackupSkipped($HOME . '/.config/opencode/local.json'))
call assert_false(&l:swapfile)
call assert_false(&l:undofile)

" A symlink to a secret gets no backup under either name. Neovim may reuse
" the secret's own buffer for the alias, so assert the outcome, not the
" pattern.
let s:backup_dir = substitute(&backupdir, '/*$', '', '')
call writefile(['token'], $HOME . '/.config/rclone/rclone.conf')
call system(['ln', '-sf', $HOME . '/.config/rclone/rclone.conf', $HOME . '/alias.conf'])
execute 'edit! ' . fnameescape($HOME . '/alias.conf')
call assert_false(&l:swapfile)
call assert_false(&l:undofile)
call setline(1, 'v2')
write
call assert_true(empty(glob(s:backup_dir . '/*alias.conf~', 0, 1)))
call assert_true(empty(glob(s:backup_dir . '/*rclone.conf~', 0, 1)))

" Visiting a secret must not switch backups off for the files that follow.
execute 'edit ' . fnameescape($HOME . '/ordinary.txt')
call assert_true(&backup)
call assert_true(&writebackup)
call assert_false(s:BackupSkipped($HOME . '/ordinary.txt'))

" Vim itself must skip the backup, not merely hold a matching pattern: an
" ordinary file gains a backup on write, a secret does not.
call writefile(['v1'], $HOME . '/ordinary.txt')
execute 'edit! ' . fnameescape($HOME . '/ordinary.txt')
call setline(1, 'v2')
write
call assert_false(empty(glob(s:backup_dir . '/*ordinary.txt~', 0, 1)))
execute 'edit! ' . fnameescape($HOME . '/.config/rclone/rclone.conf')
call setline(1, 'v2')
write
call assert_true(empty(glob(s:backup_dir . '/*rclone.conf~', 0, 1)))

if !empty(v:errors)
  for error in v:errors
    echomsg error
  endfor
  cquit
endif
quitall!
