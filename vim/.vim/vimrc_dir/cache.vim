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

" The credential paths come from lib/sensitive-paths.nix. Home Manager sets
" them as g:dotfiles_sensitive_paths before the vimrc runs: directory names
" that are sensitive at any depth of a path ('anyDepthDirectories'), and file
" and directory names relative to the home directory ('home') and to the XDG
" config directory ('xdgConfig'). Without the variable no path below is
" guarded, so report it instead of starting quietly.
if !exists('g:dotfiles_sensitive_paths')
  echoerr 'cache.vim: g:dotfiles_sensitive_paths is unset; credential files'
        \ 'will get backup, swap, and undo files'
endif

" XDG config entries apply under ~/.config and, when $XDG_CONFIG_HOME is an
" absolute path, under that directory as well: a tool that follows the
" variable writes there, and one that ignores it writes to ~/.config.
let s:roots = {'home': [expand('~')], 'xdgConfig': [expand('~/.config')]}
if $XDG_CONFIG_HOME =~# '^/'
  call add(s:roots.xdgConfig, $XDG_CONFIG_HOME)
endif

" A secret is sensitive under the name it was opened by and under the file it
" resolves to, so keep both forms of every entry.
function! s:both_forms(path) abort
  let l:path = simplify(fnamemodify(a:path, ':p'))
  return [l:path, resolve(l:path)]
endfunction

let s:sensitive_files = []
let s:sensitive_dirs = []
for [s:kind, s:kind_roots] in items(s:roots)
  let s:entries = get(get(g:, 'dotfiles_sensitive_paths', {}), s:kind, {})
  for s:root in s:kind_roots
    for s:path in get(s:entries, 'files', [])
      call extend(s:sensitive_files, s:both_forms(s:root . '/' . s:path))
    endfor
    for s:path in get(s:entries, 'directories', [])
      call extend(s:sensitive_dirs,
            \ map(s:both_forms(s:root . '/' . s:path),
            \     {_, dir -> substitute(dir, '/*$', '/', '')}))
    endfor
  endfor
endfor
call uniq(sort(s:sensitive_files))
call uniq(sort(s:sensitive_dirs))

" A credential directory such as .ssh is sensitive wherever it is: in another
" home, on a mounted backup, or copied into a project.
let s:sensitive_anywhere = map(
      \ copy(get(get(g:, 'dotfiles_sensitive_paths', {}), 'anyDepthDirectories', [])),
      \ {_, name -> '/' . name . '/'})

" 'backup' and 'writebackup' are global options, so a buffer cannot switch
" them off for itself; 'backupskip' excludes these files per name instead.
let &backupskip .= ',' . join(
      \ map(copy(s:sensitive_files), {_, path -> escape(path, ',\\')})
      \ + map(copy(s:sensitive_dirs), {_, dir -> escape(dir, ',\\') . '*'})
      \ + map(copy(s:sensitive_anywhere), {_, dir -> '*' . escape(dir, ',\\') . '*'}),
      \ ',')

function! s:is_sensitive(path) abort
  if index(s:sensitive_files, a:path) >= 0
    return 1
  endif
  for l:dir in s:sensitive_dirs
    if stridx(a:path, l:dir) == 0
      return 1
    endif
  endfor
  for l:dir in s:sensitive_anywhere
    if stridx(a:path, l:dir) >= 0
      return 1
    endif
  endfor
  return 0
endfunction

function! s:disable_sensitive_file_state() abort
  for l:raw in [expand('<afile>:p'), expand('%:p')]
    if empty(l:raw)
      continue
    endif
    let l:candidate = fnamemodify(l:raw, ':p')
    let l:path = resolve(l:candidate)
    if !s:is_sensitive(l:candidate) && !s:is_sensitive(l:path)
      continue
    endif
    setlocal noswapfile noundofile
    " Vim matches 'backupskip' against the written path, so a symlink to a
    " sensitive file needs its own entry.
    if l:candidate !=# l:path && index(split(&backupskip, ','), escape(l:candidate, ',\')) < 0
      let &backupskip .= ',' . escape(l:candidate, ',\')
    endif
  endfor
endfunction

augroup dotfiles_sensitive_file_state
  autocmd!
  " Writing an unnamed buffer names it without BufFilePost; BufWritePre still
  " runs before the undo file is written.
  autocmd BufReadPre,BufNewFile,BufFilePost,BufWritePre * call <SID>disable_sensitive_file_state()
augroup END

unlet s:backup_dir s:dir s:state_dir s:swap_dir s:undo_dir s:view_dir
unlet s:entries s:kind s:kind_roots s:roots
unlet! s:path s:root
