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

" 'backup' and 'writebackup' are global options, so a buffer cannot switch
" them off for itself; 'backupskip' excludes these files per name instead.
let &backupskip .= ',' . join(
      \ map(copy(s:sensitive_files), {_, path -> escape(path, ',\\')})
      \ + map(copy(s:sensitive_dirs), {_, dir -> escape(dir, ',\\') . '*'}), ',')

function! s:is_sensitive(path) abort
  if index(s:sensitive_files, a:path) >= 0
    return 1
  endif
  for l:dir in s:sensitive_dirs
    if stridx(a:path, l:dir) == 0
      return 1
    endif
  endfor
  return 0
endfunction

function! s:disable_sensitive_file_state() abort
  let l:buffer = fnamemodify(expand('%:p'), ':p')
  let l:path = resolve(l:buffer)
  if !s:is_sensitive(l:path)
    return
  endif
  setlocal noswapfile noundofile
  " Vim matches 'backupskip' against the buffer name, so a symlink to a
  " sensitive file needs its own entry.
  if l:buffer !=# l:path && index(split(&backupskip, ','), escape(l:buffer, ',\')) < 0
    let &backupskip .= ',' . escape(l:buffer, ',\')
  endif
endfunction

augroup dotfiles_sensitive_file_state
  autocmd!
  autocmd BufReadPre,BufNewFile * call <SID>disable_sensitive_file_state()
augroup END

unlet s:backup_dir s:dir s:state_dir s:swap_dir s:undo_dir s:view_dir
