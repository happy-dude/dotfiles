set nomore

let s:repo = fnamemodify(resolve(expand('<sfile>:p')), ':h:h:h')
execute 'source ' . fnameescape(s:repo . '/vim/.vim/vimrc_dir/search.vim')

function! s:AssertCurSearchLink() abort
  call assert_match('links to IncSearch', execute('silent highlight CurSearch'))
endfunction

call s:AssertCurSearchLink()

highlight! link CurSearch Search
doautocmd ColorScheme
call s:AssertCurSearchLink()

setlocal buftype=nofile
call setline(1, 'foo bar foo')
call feedkeys("/foo\<CR>", 'xt')
call assert_equal(1, v:hlsearch)
call feedkeys("\<C-L>", 'xt')
call assert_equal(0, v:hlsearch)

if !empty(v:errors)
  for error in v:errors
    echomsg error
  endfor
  cquit
endif

quit
