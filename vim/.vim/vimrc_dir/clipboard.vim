" clipboard settings

" Vim's native Wayland clipboard requires compositor data-control protocols.
if !has('nvim') && exists('v:clipproviders')
  function! s:wl_clipboard_available() abort
    return !empty($WAYLAND_DISPLAY) && executable('wl-copy') && executable('wl-paste')
  endfunction

  " The clipboard holds plain text, so a trailing newline is what marks a
  " linewise register, as with Vim's own clipboard; blockwise becomes
  " characterwise.
  function! s:wl_clipboard_copy(register, type, lines) abort
    let l:command = a:register ==# '*' ? 'wl-copy --primary' : 'wl-copy'
    let l:text = join(a:lines, "\n") . (a:type ==# 'V' ? "\n" : '')
    call system(l:command, l:text)
  endfunction

  function! s:wl_clipboard_paste(register) abort
    " Quote the type: the shell would otherwise end the command at ';'.
    let l:command = 'wl-paste --no-newline --type '
          \ . shellescape('text/plain;charset=utf-8')
    if a:register ==# '*'
      let l:command .= ' --primary'
    endif
    let l:text = system(l:command)
    " An empty clipboard fails with a message on stderr, which system()
    " captures; an invalid result leaves the register unchanged.
    if v:shell_error
      return []
    endif
    let l:lines = split(l:text, "\n", 1)
    if len(l:lines) > 1 && l:lines[-1] ==# ''
      return ['V', l:lines[:-2]]
    endif
    return ['v', l:lines]
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
  " Try the provider only after the native methods, where the compositor
  " lacks data-control.
  set clipmethod+=wl_clipboard
endif

if has('unnamedplus')
  set clipboard=unnamedplus   " Use '+' (the clipboard) for unnamed yank, delete, and put operations
elseif has('clipboard')
  set clipboard=unnamed       " Fall back to '*' (the primary selection on X11 and Wayland)
endif

" Highlight yanked region
"   ref: https://github.com/neovim/neovim/pull/12279
if has('nvim')
  augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua vim.hl.on_yank { higroup = 'IncSearch', timeout = 150, on_visual = false }
  augroup end
endif
