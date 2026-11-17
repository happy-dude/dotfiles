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

" Vim takes CTRL-L from search.vim; Neovim keeps its default mapping, which
" every supported release defines (:h CTRL-L-default).
let s:ctrl_l = maparg('<C-L>', 'n', 0, 1)
if has('nvim')
  call assert_equal(':help CTRL-L-default', get(s:ctrl_l, 'desc', ''),
        \ 'CTRL-L is not Neovim''s default')
else
  call assert_match('nohlsearch', get(s:ctrl_l, 'rhs', ''),
        \ 'search.vim did not map CTRL-L')
endif

" Neovim 0.13 places multicursors with Q and clears them with its default
" CTRL-L, which search.vim must leave in place (:h mcursor-clear).
if has('nvim-0.13')
  call setline(1, ['one', 'two', 'three'])
  call cursor(1, 1)
  call feedkeys('VjjQ', 'xt')
  let s:mc_ns = nvim_create_namespace('nvim.multicursor')
  call assert_equal(3, len(nvim_buf_get_extmarks(0, s:mc_ns, 0, -1, {})),
        \ 'VjjQ did not place three multicursors')
  call feedkeys("\<C-L>", 'xt')
  call assert_equal(0, len(nvim_buf_get_extmarks(0, s:mc_ns, 0, -1, {})),
        \ 'CTRL-L did not clear the multicursors')
endif

if !empty(v:errors)
  for error in v:errors
    echomsg error
  endfor
  cquit
endif

quit
