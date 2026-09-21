set nomore
let s:repo = fnamemodify(resolve(expand('<sfile>:p')), ':h:h:h')
execute 'source ' . fnameescape(s:repo . '/vim/.vim/vimrc_dir/functions.vim')

function! s:Fixture(lines) abort
  enew!
  call setline(1, a:lines)
  " Start a new undo block after creating the fixture.
  let &undolevels = &undolevels
endfunction

call s:Fixture(['first', 'middle', 'last'])
call setreg('"', 'preserved register')
2S/middle/one\ntwo/
call assert_equal(['first', 'one', 'two', 'last'], getline(1, '$'))
call assert_equal('preserved register', getreg('"'))
undo
call assert_equal(['first', 'middle', 'last'], getline(1, '$'))

call s:Fixture(['first', 'middle', 'last'])
1,2S/\n//
call assert_equal(['firstmiddle', 'last'], getline(1, '$'))
undo
call assert_equal(['first', 'middle', 'last'], getline(1, '$'))

call s:Fixture(['first', 'middle', 'last'])
2S/.*//
call assert_equal(['first', 'last'], getline(1, '$'))
undo
call assert_equal(['first', 'middle', 'last'], getline(1, '$'))

" Failed Perl input must not replace the addressed text with diagnostics.
call s:Fixture(['first', 'middle', 'last'])
2S/[/invalid/
call assert_equal(['first', 'middle', 'last'], getline(1, '$'))

if !empty(v:errors)
  for error in v:errors
    echomsg error
  endfor
  cquit
endif
quitall!
