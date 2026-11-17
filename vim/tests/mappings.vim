" Loaded by the vim-startup check after the profile's configuration. Each
" assertion names a key whose mapping this configuration owns: the restored
" stock keys must stay unmapped, and the CoC keys that vim-rsi also maps must
" come from after/plugin/coc.vim, which runs after vim-rsi's plugin script.
function! s:Source(mode, lhs) abort
  let l:map = maparg(a:lhs, a:mode, 0, 1)
  let l:script = get(getscriptinfo({'sid': get(l:map, 'sid', 0)}), 0, {})
  return get(l:script, 'name', '')
endfunction

for [s:mode, s:lhs] in [['i', '<End>'], ['n', '<End>'], ['i', '<Home>'],
      \ ['n', '<Home>'], ['o', '<Down>'], ['o', '<Up>'], ['o', 'z']]
  call assert_equal('', maparg(s:lhs, s:mode),
        \ s:mode . ' ' . s:lhs . ' is mapped')
endfor

" CoC maps the Insert-mode arrows for its popup menu only when nothing else
" has, so the configuration must leave them alone.
for s:lhs in ['<Down>', '<Up>']
  call assert_notmatch('/vimrc_dir/', s:Source('i', s:lhs),
        \ 'i ' . s:lhs . ' is mapped by the configuration')
endfor

" The runtime directory is a Nix store path ending in -after.
for s:lhs in ['<C-e>', '<C-f>', '<C-b>']
  call assert_match('[/-]after/plugin/coc\.vim$', s:Source('i', s:lhs),
        \ 'i ' . s:lhs . ' is not mapped from after/plugin/coc.vim')
endfor

if !empty(v:errors)
  call writefile(v:errors, '/dev/stderr')
  cquit
endif
