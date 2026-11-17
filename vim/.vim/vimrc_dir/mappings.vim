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

" Never "accidentally" enter Ex mode. Vim enters it with Q and gQ. Neovim
" 0.13 (nightly) enters it with [count]q:, which the q: mapping below blocks,
" and gives Q and gQ to multicursors. ref: :h Q, :h Ex-mode
if !has('nvim')
  nnoremap  Q           <nop>
  nnoremap  gQ          <nop>
endif

" Command history; use CTRL-F instead while in command mode
nnoremap    q:          <nop>
"
" Search history; use CTRL-F instead while in search mode
nnoremap    q/          <nop>

" From https://github.com/tommcdo/vimfiles/blob/master/config/consistency.vim
" Make Y behave like C and D
nnoremap    Y           y$

" "cw" stops at the end of the word. To make it include the following white
" space like "dw", remove the z flag from 'cpoptions' in Vim or the _ flag in
" Neovim; "caw" does it once. In Vim, removing z also disables the linewise
" d-special exception. ref: :h cpo-z, :h cpo-_, :h d-special

" From https://stackoverflow.com/posts/3879737/revisions
" :hs command abbreviation/ alias for :split (horizontal split)
" Provides some consistency for :vs (shorthand for :vsplit, vertical split)
cnoreabbrev <expr> hs ((getcmdtype() is# ':' && getcmdline() is# 'hs')?('split'):('hs'))
