" search and grep settings

set ignorecase          " Ignore case when searching
set smartcase           " If there are caps, go case-sensitive
set infercase           " Infer keyword-completion case
set hlsearch            " Highlight search matches
set incsearch           " Highlight matches while entering a search

function! s:ApplySearchHighlights() abort
  " Distinguish the current search match from the other highlighted matches.
  highlight! link CurSearch IncSearch
endfunction

call s:ApplySearchHighlights()

augroup SearchHighlights
  autocmd!
  autocmd ColorScheme * call <SID>ApplySearchHighlights()
augroup END

" Give Vim Neovim's default CTRL-L: clear the search highlight, update diffs
" in diff mode, and redraw. A function restores the search highlight state
" when it returns, so :nohlsearch has to run from the mapping itself. Neovim
" keeps its own default, which runs :diffupdate unconditionally and also
" clears multicursors. ref: :h CTRL-L-default
if !has('nvim')
  nnoremap <silent> <C-L> <Cmd>nohlsearch<Bar>if &diff<Bar>diffupdate<Bar>endif<CR><C-L>
endif

if executable('rg')
  let &grepprg = 'rg --color=never --vimgrep --no-heading --smart-case --hidden --glob '
        \ . shellescape('!.git/*')
  set grepformat=%f:%l:%c:%m
else
  let &grepprg = 'grep -nH -R $* /dev/null'
endif
