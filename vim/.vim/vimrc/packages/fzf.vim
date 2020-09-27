" fzf.vim settings

let g:airline#extensions#fzf#enabled = 1      " Enable airline fzf integration

" Display fzf in a split when using an older version of neovim or vim
if !has('nvim-0.4.0') || (!has("patch-8.2.0191") && !has('nvim'))
  let g:fzf_layout = { "window": "silent botright 12split enew" }
endif

" Delegate search responsibliity to ripgrep by restarting ripgrep whenever
" query string is updated. fzf becomes a simple selector interface
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

" git grep
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep(
  \   'git grep --line-number -- '.shellescape(<q-args>), 0,
  \   fzf#vim#with_preview({'dir': systemlist('git rev-parse --show-toplevel')[0]}), <bang>0)

" Search and apply mappings on selection objects
nmap      <leader><tab>             <plug>(fzf-maps-n)
xmap      <leader><tab>             <plug>(fzf-maps-x)
omap      <leader><tab>             <plug>(fzf-maps-o)

" Insert mode completion search
imap      <c-x><c-k>                <plug>(fzf-complete-word)
imap      <c-x><c-f>                <plug>(fzf-complete-path)
imap      <c-x><c-l>                <plug>(fzf-complete-line)

" Command mappings
nnoremap  <silent> <leader><space> :Files<CR>
nnoremap  <silent> <leader>g       :GFiles?<CR>
nnoremap  <silent> <leader>b       :Buffers<CR>
nnoremap  <silent> <leader>l       :Lines<CR>
nnoremap  <silent> <leader>m       :Marks<CR>
nnoremap  <silent> <leader>rg      :Rg<CR>
nnoremap  <silent> <leader>ex      :History:<CR>
nnoremap  <silent> <leader>s       :History/<CR>
