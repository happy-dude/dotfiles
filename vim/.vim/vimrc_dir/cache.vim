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

let s:sensitive_files = map([
      \ '~/.authinfo',
      \ '~/.authinfo.gpg',
      \ '~/.claude.json',
      \ '~/.config/fish/secrets.fish',
      \ '~/.config/git/local.config',
      \ '~/.config/nix/local.conf',
      \ '~/.netrc',
      \ ], {_, path -> resolve(fnamemodify(expand(path), ':p'))})
let s:sensitive_dirs = map([
      \ '~/.aws',
      \ '~/.claude',
      \ '~/.codex',
      \ '~/.config/gh',
      \ '~/.config/op',
      \ '~/.config/opencode',
      \ '~/.config/rclone',
      \ '~/.gnupg',
      \ '~/.kube',
      \ '~/.password-store',
      \ '~/.ssh',
      \ ], {_, path -> resolve(fnamemodify(expand(path), ':p')) . '/'})

function! s:disable_sensitive_file_state() abort
  let l:path = resolve(fnamemodify(expand('%:p'), ':p'))
  if index(s:sensitive_files, l:path) >= 0
    setlocal nobackup nowritebackup noswapfile noundofile
    return
  endif
  for l:dir in s:sensitive_dirs
    if stridx(l:path, l:dir) == 0
      setlocal nobackup nowritebackup noswapfile noundofile
      return
    endif
  endfor
endfunction

augroup dotfiles_sensitive_file_state
  autocmd!
  autocmd BufReadPre,BufNewFile * call <SID>disable_sensitive_file_state()
augroup END

unlet s:backup_dir s:dir s:state_dir s:swap_dir s:undo_dir s:view_dir
