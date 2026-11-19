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
" config and data directories ('xdgConfig', 'xdgData'). Without the variable
" no path below is guarded, so report it instead of starting quietly.
if !exists('g:dotfiles_sensitive_paths')
  echoerr 'cache.vim: g:dotfiles_sensitive_paths is unset; credential files'
        \ 'will get backup, swap, and undo files'
  finish
endif

" Guard the standard roots even when XDG relocates them: old credentials
" can remain there, and not every program follows the XDG variables.
let s:roots = {'home': [expand('~')], 'xdgConfig': [expand('~/.config')],
      \ 'xdgData': [expand('~/.local/share')]}
if $XDG_CONFIG_HOME =~# '^/'
  call add(s:roots.xdgConfig, $XDG_CONFIG_HOME)
endif
if $XDG_DATA_HOME =~# '^/'
  call add(s:roots.xdgData, $XDG_DATA_HOME)
endif

" Store the declared names, not their current targets: a credential root
" can be created or retargeted after the editor has started.
let s:paths = get(g:, 'dotfiles_sensitive_paths', {})
let s:sensitive_files = []
let s:sensitive_dirs = []
for [s:kind, s:kind_roots] in items(s:roots)
  let s:entries = get(s:paths, s:kind, {})
  let s:directories = get(s:entries, 'directories', [])
  if s:kind ==# 'home'
    let s:directories = s:directories + get(s:paths, 'anyDepthDirectories', [])
  endif
  for s:root in s:kind_roots
    for s:path in get(s:entries, 'files', [])
      call add(s:sensitive_files, simplify(s:root . '/' . s:path))
    endfor
    for s:path in s:directories
      call add(s:sensitive_dirs, simplify(s:root . '/' . s:path) . '/')
    endfor
  endfor
endfor
call uniq(sort(s:sensitive_files))
call uniq(sort(s:sensitive_dirs))

" A credential directory such as .ssh is sensitive wherever it is: in another
" home, on a mounted backup, or copied into a project.
let s:sensitive_anywhere = map(
      \ copy(get(s:paths, 'anyDepthDirectories', [])),
      \ {_, name -> '/' . name . '/'})

" 'backup' and 'writebackup' are global options, so a buffer cannot switch
" them off for itself; 'backupskip' excludes these files per name instead.
let &backupskip .= ',' . join(
      \ map(copy(s:sensitive_files), {_, path -> escape(path, ',\\')})
      \ + map(copy(s:sensitive_dirs), {_, dir -> escape(dir, ',\\') . '*'})
      \ + map(copy(s:sensitive_anywhere), {_, dir -> '*' . escape(dir, ',\\') . '*'}),
      \ ',')

function! s:is_sensitive(path) abort
  let l:names = [a:path, resolve(a:path)]
  for l:name in l:names
    for l:dir in s:sensitive_anywhere
      if stridx(l:name, l:dir) >= 0
        return 1
      endif
    endfor
  endfor
  for l:file in s:sensitive_files
    if index(l:names, l:file) >= 0 || index(l:names, resolve(l:file)) >= 0
      return 1
    endif
  endfor
  for l:dir in s:sensitive_dirs
    let l:target = substitute(resolve(l:dir), '/*$', '/', '')
    for l:name in l:names
      if stridx(l:name, l:dir) == 0 || stridx(l:name, l:target) == 0
        return 1
      endif
    endfor
  endfor
  return 0
endfunction

function! s:update_sensitive_file_state(event_name) abort
  for l:name in [a:event_name, bufname('%')]
    if !empty(l:name) && s:is_sensitive(fnamemodify(l:name, ':p'))
      if !exists('b:dotfiles_saved_state')
        let b:dotfiles_saved_state = [&l:swapfile, &l:undofile]
      endif
      setlocal noswapfile noundofile
      return 1
    endif
  endfor
  if exists('b:dotfiles_saved_state')
    let [&l:swapfile, &l:undofile] = b:dotfiles_saved_state
    unlet b:dotfiles_saved_state
  endif
  return 0
endfunction

" 'backupskip' matches the written name, not its resolved target. Scope
" the extra exclusion to this write, not every alias ever visited. A failed
" write need not run its Post hook, so also restore before the next write.
function! s:finish_write() abort
  if exists('s:previous_backupskip')
    if &backupskip ==# '*'
      let &backupskip = s:previous_backupskip
    endif
    unlet s:previous_backupskip
  endif
endfunction

function! s:prepare_write() abort
  call s:finish_write()
  if s:update_sensitive_file_state(expand('<afile>:p'))
    let s:previous_backupskip = &backupskip
    set backupskip=*
  endif
endfunction

augroup dotfiles_sensitive_file_state
  autocmd!
  autocmd BufReadPre,BufNewFile * call <SID>update_sensitive_file_state(expand('<afile>:p'))
  " Entering an unnamed scratch buffer has no <afile>. Only file events
  " supply a path in addition to the current buffer's name.
  autocmd BufFilePost,BufEnter,InsertEnter * call <SID>update_sensitive_file_state('')
  autocmd BufWritePre,FileWritePre,FileAppendPre * call <SID>prepare_write()
  autocmd BufWritePost,FileWritePost,FileAppendPost * call <SID>finish_write()
augroup END

unlet s:backup_dir s:dir s:state_dir s:swap_dir s:undo_dir s:view_dir
unlet s:directories s:entries s:kind s:kind_roots s:roots s:paths
unlet! s:path s:root
