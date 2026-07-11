" persistent editor state

if has('nvim')
  let s:state_dir = stdpath('state')
else
  let s:state_dir = expand('~/.cache/vim')
endif

let s:backup_dir = s:state_dir . '/backup'
let s:swap_dir = s:state_dir . '/swap'
let s:undo_dir = s:state_dir . '/undo'
let s:view_dir = s:state_dir . '/view'

for s:dir in [s:backup_dir, s:swap_dir, s:undo_dir, s:view_dir]
  call mkdir(s:dir, 'p', 0700)
  if has('unix')
    call setfperm(s:dir, 'rwx------')
  endif
endfor

let &backupdir = s:backup_dir . '//'
let &directory = s:swap_dir . '//'
let &undodir = s:undo_dir . '//'
let &viewdir = s:view_dir . '//'

set backup
set writebackup
set swapfile
set undofile

unlet s:backup_dir s:dir s:state_dir s:swap_dir s:undo_dir s:view_dir
