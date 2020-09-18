" custom mappings

" Map Ctrl-Backspace to delete previous word in insert mode like Ctrl-w
" Note how this mapping does not work in terminal vim because of term-keys
" However, this mapping will work just fine in gvim
if has("gui_running")
    inoremap    <C-BS>      <C-w>
endif

" To move up and down logical lines instead of physical lines
" Instead of changing the Home row keys, use the arrow keys
nnoremap    <expr><Down>            (v:count == 0 ? 'gj' : 'j')
nnoremap    <expr><Up>              (v:count == 0 ? 'gk' : 'k')
vnoremap    <expr><Down>            (v:count == 0 ? 'gj' : 'j')
vnoremap    <expr><Up>              (v:count == 0 ? 'gk' : 'k')
noremap     <silent><expr><Down>    (v:count == 0 ? 'gj' : 'j')
noremap     <silent><expr><Up>      (v:count == 0 ? 'gk' : 'k')
inoremap    <Down>                  <C-o>gj
inoremap    <Up>                    <C-o>gk
inoremap    <silent><Up>            <C-o>gk
inoremap    <silent><Down>          <C-o>gj
" Home and end keys are broken -- dependent of TERM variable;
" Not sure how to fix reliably
noremap     <silent><Home>          g<Home>
noremap     <silent><End>           g<End>
inoremap    <silent><Home>          <C-o>g<Home>
inoremap    <silent><End>           <C-o>g<End>

" Smart way to move between windows
" Also noted how they don't seem to work... Need to investigate.
"noremap    <C-j>       <C-w>j
"noremap    <C-k>       <C-w>k
"noremap    <C-h>       <C-w>h
"noremap    <C-l>       <C-w>l

" Never "accidentally" enter ex mode
nnoremap    Q           <nop>

" From https://github.com/tommcdo/vimfiles/blob/master/config/consistency.vim
" Make Y behave like C and D
nnoremap    Y           y$

" Make cw behave like dw and yw
" NOTE: This causes some weird behaviour for end-of-line edge cases:
"   https://asciinema.org/a/9843
" To compensate for this edge case, use caw
"onoremap <silent> w :execute 'normal! '.v:count1.'w'<CR>

" From http://www.reddit.com/r/vim/comments/26nut8/why_does_cw_work_like_ce/chsz0pq
" What this does is it creates a new operator "z" that forcibly acts as a
" normal "w". This "w" is then mapped (only for cw, aka the "error") to cw to
" fix it. However it works perfectly fine with dw as well. It takes just one
" command history so undo works properly and dot repetition works as expected.
onoremap    <silent>z   :<C-U>normal! w<CR>
"map        cw          cz

" From https://stackoverflow.com/posts/3879737/revisions
" :hs command abbreviation/ alias for :split (horizontal split)
" Provides some consistency for :vs (shorthand for :vsplit, vertical split)
cnoreabbrev <expr> hs ((getcmdtype() is# ':' && getcmdline() is# 'hs')?('split'):('hs'))
