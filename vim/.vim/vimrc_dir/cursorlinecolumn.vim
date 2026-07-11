" cursor, cursorline, and cursorcolumn settings

set ruler
set cursorline
set cursorcolumn
set linespace=0
set colorcolumn=81
set signcolumn=number
set relativenumber
set number
set numberwidth=5

" Switch between absolute and relative numbers as editing context changes.
augroup dynamic_line_numbers
  autocmd!
  autocmd FocusLost * setlocal number norelativenumber
  autocmd FocusGained * setlocal relativenumber
  autocmd InsertEnter * setlocal number norelativenumber
  autocmd InsertLeave * setlocal relativenumber
augroup END
