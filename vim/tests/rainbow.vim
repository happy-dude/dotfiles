" Loaded by the vim-startup check after the profile's configuration, with
" DOTFILES_RAINBOW_PALETTE naming lib/rainbow-delimiters.nix as JSON. The
" expected colours come from that file rather than the variable rainbow.vim
" reads, so a wrong preamble cannot agree with itself.
let s:palette = json_decode(join(readfile($DOTFILES_RAINBOW_PALETTE), "\n"))
let s:gui = map(copy(s:palette), 'tolower(v:val.gui)')
let s:cterm = map(copy(s:palette), 'string(v:val.cterm)')
call assert_notequal([], s:palette, 'the rainbow palette is empty')

" The configuration the plugin resolves. Its defaults give lisp colours of
" its own and disable sh, and the empty '*' entry in rainbow.vim must
" override both; tex and vim have entries there, and c has none anywhere.
for s:filetype in ['c', 'lisp', 'sh', 'tex', 'vim']
  let s:conf = rainbow_main#gen_config(s:filetype)
  if type(s:conf) != v:t_dict
    call add(v:errors, s:filetype . ': rainbow is disabled')
    continue
  endif
  call assert_equal(s:gui, map(copy(s:conf.guifgs), 'tolower(v:val)'),
        \ s:filetype . ' guifgs')
  call assert_equal(s:cterm, s:conf.ctermfgs, s:filetype . ' ctermfgs')
endfor

" The highlights the plugin applies to a Lisp buffer. Silent Ex mode does
" not fire its Syntax autocommand, so load it directly; :colorscheme clears
" the groups, and the plugin's ColorScheme autocommand must restore them.
function! s:AssertHighlights(when) abort
  for l:depth in range(len(s:palette))
    let l:id = hlID('lispRainbow_lv' . l:depth . '_p0')
    let l:label = 'depth ' . (l:depth + 1) . ' ' . a:when
    call assert_equal(s:gui[l:depth], tolower(synIDattr(l:id, 'fg', 'gui')),
          \ l:label)
    call assert_equal(s:cterm[l:depth], synIDattr(l:id, 'fg', 'cterm'),
          \ l:label)
  endfor
endfunction
enew
setfiletype lisp
call rainbow_main#load()
call s:AssertHighlights('after loading')
execute 'colorscheme' g:colors_name
call s:AssertHighlights('after :colorscheme')

" Sourced without the Home Manager preamble, rainbow.vim sets no colours,
" leaving the plugin a default it can use rather than an empty list.
unlet g:dotfiles_rainbow_palette
execute 'source'
      \ fnameescape(g:dotfiles_vim_runtime . '/vimrc_dir/packages/rainbow.vim')
call assert_false(has_key(g:rainbow_conf, 'guifgs'),
      \ 'rainbow.vim set colours without a palette')
try
  call rainbow_main#load()
catch
  call add(v:errors, 'without a palette: ' . v:exception)
endtry

if !empty(v:errors)
  call writefile(v:errors, '/dev/stderr')
  cquit
endif
