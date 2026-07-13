" clipboard settings

" Vim's native Wayland clipboard requires compositor data-control protocols.
if !has('nvim') && exists('v:clipproviders')
  function! s:wl_clipboard_available() abort
    return !empty($WAYLAND_DISPLAY) && executable('wl-copy') && executable('wl-paste')
  endfunction

  function! s:wl_clipboard_copy(register, type, lines) abort
    let l:command = a:register ==# '*' ? 'wl-copy --primary' : 'wl-copy'
    call system(l:command, a:lines)
  endfunction

  function! s:wl_clipboard_paste(register) abort
    let l:command = 'wl-paste --type text/plain;charset=utf-8'
    if a:register ==# '*'
      let l:command .= ' --primary'
    endif
    return ['', systemlist(l:command)]
  endfunction

  let v:clipproviders['wl_clipboard'] = {
        \ 'available': function('s:wl_clipboard_available'),
        \ 'copy': {
        \   '+': function('s:wl_clipboard_copy'),
        \   '*': function('s:wl_clipboard_copy'),
        \ },
        \ 'paste': {
        \   '+': function('s:wl_clipboard_paste'),
        \   '*': function('s:wl_clipboard_paste'),
        \ },
        \ }
  set clipmethod^=wl_clipboard
endif

if has('unnamedplus')
  set clipboard=unnamedplus   " Use the '+' register for all yank, delete, and change operations; the '+' register is the X Windows clipboard
elseif has('clipboard')
  set clipboard=unnamed       " Use the '*' register for all yank, delete, and change operations; the '*' register is the system clipboard
endif

" Highlight yanked region
"   ref: https://github.com/neovim/neovim/pull/12279
if has('nvim')
  augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua vim.hl.hl_op { higroup = 'IncSearch', timeout = 150, on_visual = false }
  augroup end
endif
