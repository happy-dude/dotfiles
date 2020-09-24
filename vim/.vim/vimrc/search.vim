" search and grep settings

set ignorecase          " Ignore case when searching
set smartcase           " If there are caps, go case-sensitive
set infercase           " Case inferred by default
set hlsearch            " Highlight search things
set incsearch           " Make search act like search in modern browsers

set magic               " Set magic on, for regular expressions

" vim grep
if executable('rg')
  " if the full path of working buffer matches regex '/\/.',
  " which is a hidden file, directory, or within a hidden directory
  " set grep program with '--hidden' flag
  if expand('%:p') =~# '\/\.'
    set grepprg=rg\ --color=never\ --vimgrep\ --no-heading\ --smart-case\ --hidden
  else
    set grepprg=rg\ --color=never\ --vimgrep\ --no-heading\ --smart-case
  endif

  set grepformat=%f:%l:%c:%m
else
  let &grepprg='grep -nH -R --exclude=' . shellescape(&wildignore) . ' $* /dev/null'
endif
