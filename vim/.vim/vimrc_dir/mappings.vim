" custom mappings

" Default leader: \ (backslash)
" Local leader: , (comma) -- recommended for conjure and vim REPL environments
let maplocalleader = ","

" Delete the previous word when the frontend reports distinct Ctrl-Backspace.
inoremap <C-BS> <C-w>

" Arrow keys follow screen lines in Normal, Visual, and Select mode; an
" explicit count uses buffer lines. After an operator they stay the linewise
" j and k, because gj and gk are characterwise motions.
nnoremap    <silent><expr><Down>    (v:count == 0 ? 'gj' : 'j')
vnoremap    <silent><expr><Down>    (v:count == 0 ? 'gj' : 'j')
nnoremap    <silent><expr><Up>      (v:count == 0 ? 'gk' : 'k')
vnoremap    <silent><expr><Up>      (v:count == 0 ? 'gk' : 'k')

" Optional window-navigation mappings; left disabled.
"noremap    <C-j>       <C-w>j
"noremap    <C-k>       <C-w>k
"noremap    <C-h>       <C-w>h
"noremap    <C-l>       <C-w>l

" Never "accidentally" enter ex mode
nnoremap    Q           <nop>

" Command history; use CTRL-F instead while in command mode
nnoremap    q:          <nop>
"
" Search history; use CTRL-F instead while in search mode
nnoremap    q/          <nop>

" From https://github.com/tommcdo/vimfiles/blob/master/config/consistency.vim
" Make Y behave like C and D
nnoremap    Y           y$

" Make cw behave like dw and yw
" NOTE: This causes some weird behaviour for end-of-line edge cases:
"   https://asciinema.org/a/9843
" To compensate for this edge case, use caw
"onoremap <silent> w :execute 'normal! '.v:count1.'w'<CR>

" From http://www.reddit.com/r/vim/comments/26nut8/why_does_cw_work_like_ce/chsz0pq
" Operator-pending z uses the normal w motion. The optional cw mapping below
" is disabled, so cw retains Vim's built-in behaviour.
onoremap    <silent>z   :<C-U>normal! w<CR>
"map        cw          cz

" From https://stackoverflow.com/posts/3879737/revisions
" :hs command abbreviation/ alias for :split (horizontal split)
" Provides some consistency for :vs (shorthand for :vsplit, vertical split)
cnoreabbrev <expr> hs ((getcmdtype() is# ':' && getcmdline() is# 'hs')?('split'):('hs'))
