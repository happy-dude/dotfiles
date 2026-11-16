" Loaded by the vim-startup check after the profile's configuration, with
" DOTFILES_SENSITIVE_PATHS naming lib/sensitive-paths.nix as JSON. The Home
" Manager preamble must hand cache.vim that list before the vimrc runs, so
" compare the variable with the file, then look for every entry in
" 'backupskip', which cache.vim fills while the vimrc is sourced.
" tests/secret-state.vim covers what cache.vim does with the list.
let s:paths = json_decode(join(readfile($DOTFILES_SENSITIVE_PATHS), "\n"))
call assert_equal(s:paths, get(g:, 'dotfiles_sensitive_paths', {}),
      \ 'the preamble does not carry the sensitive path list')

let s:skipped = split(&backupskip, ',')
call assert_notequal([], s:paths.anyDepthDirectories, 'any-depth directories')
for s:name in s:paths.anyDepthDirectories
  call assert_true(index(s:skipped, '*/' . s:name . '/*') >= 0,
        \ s:name . ' is not in backupskip')
endfor
for [s:kind, s:root] in [
      \ ['home', expand('~')],
      \ ['xdgConfig', expand('~/.config')],
      \ ]
  call assert_notequal([], s:paths[s:kind].files, s:kind . ' files')
  call assert_notequal([], s:paths[s:kind].directories, s:kind . ' directories')
  for s:name in s:paths[s:kind].files
    call assert_true(index(s:skipped, s:root . '/' . s:name) >= 0,
          \ s:name . ' is not in backupskip')
  endfor
  for s:name in s:paths[s:kind].directories
    call assert_true(index(s:skipped, s:root . '/' . s:name . '/*') >= 0,
          \ s:name . ' is not in backupskip')
  endfor
endfor

if !empty(v:errors)
  call writefile(v:errors, '/dev/stderr')
  cquit
endif
