set nomore
" The default patterns cover the temporary directory this test runs in;
" only the entries cache.vim adds are under test.
set backupskip=
" DOTFILES_SENSITIVE_PATHS names lib/sensitive-paths.nix as JSON. It stands in
" for the Home Manager preamble here, and is the list of paths to probe; the
" startup checks compare the real preamble with the same file.
let s:paths = json_decode(join(readfile($DOTFILES_SENSITIVE_PATHS), "\n"))
let g:dotfiles_sensitive_paths = deepcopy(s:paths)
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

function! s:AssertState(path, kept) abort
  call mkdir(fnamemodify(a:path, ':h'), 'p')
  execute 'edit! ' . fnameescape(a:path)
  call assert_equal(!a:kept, s:BackupSkipped(a:path), a:path . ' backup')
  call assert_equal(a:kept, &l:swapfile, a:path . ' swap file')
  call assert_equal(a:kept, &l:undofile, a:path . ' undo file')
endfunction

execute 'edit ' . fnameescape($HOME . '/ordinary.txt')
call assert_true(&l:backup)
call assert_true(&l:writebackup)
call assert_true(&l:swapfile)
call assert_true(&l:undofile)

" Every entry is guarded wherever it can live: home entries under the home
" directory, XDG config entries under ~/.config and under the relocated
" XDG_CONFIG_HOME the check exports.
let s:config = $XDG_CONFIG_HOME
call assert_match('^/', s:config, 'XDG_CONFIG_HOME is not absolute')
call assert_notequal($HOME . '/.config', s:config,
      \ 'the check did not relocate XDG_CONFIG_HOME')
for [s:kind, s:roots] in [
      \ ['home', [$HOME]],
      \ ['xdgConfig', [$HOME . '/.config', s:config]],
      \ ]
  call assert_notequal([], s:paths[s:kind].files, s:kind . ' files')
  call assert_notequal([], s:paths[s:kind].directories, s:kind . ' directories')
  for s:root in s:roots
    for s:name in s:paths[s:kind].files
      call s:AssertState(s:root . '/' . s:name, 0)
      " A file entry is one file, not a name prefix.
      call s:AssertState(s:root . '/' . s:name . '.sample', 1)
    endfor
    for s:name in s:paths[s:kind].directories
      call s:AssertState(s:root . '/' . s:name . '/credential.txt', 0)
      call s:AssertState(s:root . '/' . s:name . '/nested/credential.txt', 0)
      " A directory entry ends at a path separator.
      call s:AssertState(s:root . '/' . s:name . '-notes/ordinary.txt', 1)
    endfor
  endfor
endfor

" A credential directory is guarded at any depth: in the home directory,
" in a tree outside it such as another home or a mounted backup, and inside
" a project. A name that merely starts the same way is not.
let s:elsewhere = fnamemodify($HOME, ':h') . '/elsewhere'
call assert_notequal([], s:paths.anyDepthDirectories, 'any-depth directories')
for s:name in s:paths.anyDepthDirectories
  for s:root in [$HOME, s:elsewhere, $HOME . '/project', s:elsewhere . '/deep/tree']
    call s:AssertState(s:root . '/' . s:name . '/credential.txt', 0)
    call s:AssertState(s:root . '/' . s:name . '/nested/credential.txt', 0)
    call s:AssertState(s:root . '/' . s:name . '-notes/ordinary.txt', 1)
  endfor
endfor

" Vim itself must leave no copy there: writing a file under a credential
" directory outside the home directory gains no backup and no undo file.
let s:backup_dir = substitute(&backupdir, '/*$', '', '')
for s:path in [s:elsewhere . '/.ssh/id_ed25519', $HOME . '/project/.aws/credentials']
  execute 'edit! ' . fnameescape(s:path)
  call assert_false(&l:swapfile, s:path . ' swap file')
  call setline(1, 'token')
  write
  call assert_true(empty(glob(s:backup_dir . '/*' . fnamemodify(s:path, ':t') . '~', 0, 1)),
        \ s:path . ' backup')
  call assert_false(filereadable(undofile(s:path)), s:path . ' undo file')
endfor

" The other entries are anchored: a directory of the same name elsewhere,
" such as a project's own .claude or .config, keeps its state.
for s:name in s:paths.home.directories
  call s:AssertState($HOME . '/project/' . s:name . '/ordinary.txt', 1)
endfor
for s:name in s:paths.xdgConfig.directories
  call s:AssertState($HOME . '/project/.config/' . s:name . '/ordinary.txt', 1)
endfor

" The scenarios below use one directory entry under the relocated root.
let s:rclone = s:config . '/rclone'

" A symlink to a secret gets no backup under either name. Neovim may reuse
" the secret's own buffer for the alias, so assert the outcome, not the
" pattern.
call writefile(['token'], s:rclone . '/rclone.conf')
call system(['ln', '-sf', s:rclone . '/rclone.conf', $HOME . '/alias.conf'])
execute 'edit! ' . fnameescape($HOME . '/alias.conf')
call assert_false(&l:swapfile)
call assert_false(&l:undofile)
call setline(1, 'v2')
write
call assert_true(empty(glob(s:backup_dir . '/*alias.conf~', 0, 1)))
call assert_true(empty(glob(s:backup_dir . '/*rclone.conf~', 0, 1)))

" A secret's name stays sensitive when it links to a file outside every
" sensitive directory.
call mkdir($HOME . '/private', 'p')
call writefile(['token'], $HOME . '/private/token')
call system(['ln', '-sf', $HOME . '/private/token', s:rclone . '/linked.conf'])
execute 'edit! ' . fnameescape(s:rclone . '/linked.conf')
call assert_false(&l:swapfile)
call assert_false(&l:undofile)
call setline(1, 'v2')
write
call assert_false(filereadable(undofile(resolve(expand('%:p')))))

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
call assert_true(filereadable(undofile($HOME . '/ordinary.txt')))
execute 'edit! ' . fnameescape(s:rclone . '/rclone.conf')
call setline(1, 'v2')
write
call assert_true(empty(glob(s:backup_dir . '/*rclone.conf~', 0, 1)))

" :saveas changes a buffer's identity without reading a new file. Its new
" sensitive name must disable state before the write persists undo history.
execute 'edit ' . fnameescape($HOME . '/ordinary.txt')
call assert_true(&l:swapfile)
call assert_true(&l:undofile)
execute 'saveas! ' . fnameescape(s:rclone . '/renamed.txt')
call assert_false(&l:swapfile)
call assert_false(&l:undofile)
call assert_false(filereadable(undofile(expand('%:p'))))

" Writing an unnamed buffer gives it the file's name without BufFilePost.
" Both editors run in the same HOME, so the second one overwrites the file.
enew!
call setline(1, 'token')
execute 'write! ' . fnameescape(s:rclone . '/written.conf')
call assert_false(&l:swapfile)
call assert_false(&l:undofile)
call assert_false(filereadable(undofile(expand('%:p'))))


" Writing an already-named ordinary buffer to a sensitive path keeps % as the
" ordinary name while <afile> names the sensitive destination.
execute 'edit! ' . fnameescape($HOME . '/ordinary.txt')
call assert_true(&l:swapfile)
call assert_true(&l:undofile)
call setline(1, 'secret-copy')
execute 'write! ' . fnameescape(s:rclone . '/from-ordinary.conf')
call assert_false(&l:swapfile)
call assert_false(&l:undofile)
call assert_false(filereadable(undofile(s:rclone . '/from-ordinary.conf')))

" A relative XDG_CONFIG_HOME names no config directory, so it adds no entry:
" a file below it keeps its state, and ~/.config is still guarded.
let $XDG_CONFIG_HOME = 'relative-config'
set backupskip=
execute 'source ' . fnameescape($DOTFILES_CACHE_VIM)
for s:name in s:paths.xdgConfig.directories
  call s:AssertState(getcwd() . '/relative-config/' . s:name . '/ordinary.txt', 1)
  call s:AssertState($HOME . '/.config/' . s:name . '/second.txt', 0)
  call s:AssertState(s:config . '/' . s:name . '/unguarded.txt', 1)
endfor

" Sourced without the Home Manager preamble, cache.vim has no paths to guard
" and must say so.
unlet g:dotfiles_sensitive_paths
try
  execute 'source ' . fnameescape($DOTFILES_CACHE_VIM)
  call add(v:errors, 'cache.vim loaded quietly without its path list')
catch /g:dotfiles_sensitive_paths is unset/
endtry

if !empty(v:errors)
  for error in v:errors
    echomsg error
  endfor
  cquit
endif
quitall!
