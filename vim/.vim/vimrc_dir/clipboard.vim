" clipboard settings

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
