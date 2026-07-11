" search and grep settings

set ignorecase          " Ignore case when searching
set smartcase           " If there are caps, go case-sensitive
set infercase           " Infer keyword-completion case
set hlsearch            " Highlight search matches
set incsearch           " Highlight matches while entering a search

function! s:ClearSearchHighlight() abort
  if exists('*HLNextOff')
    call HLNextOff()
  endif

  nohlsearch
  if &diff
    diffupdate
  endif
endfunction

nnoremap <silent> <C-L> <Cmd>call <SID>ClearSearchHighlight()<CR><C-L>

if executable('rg')
  let &grepprg = 'rg --color=never --vimgrep --no-heading --smart-case --hidden --glob '
        \ . shellescape('!.git/*')
  set grepformat=%f:%l:%c:%m
else
  let &grepprg = 'grep -nH -R $* /dev/null'
endif
