" cursor, cursorline, and cursorcolumn settings

set ruler               " Show cursor position all times
set cursorline          " Highlight the current line
set cursorcolumn        " Highlight the current column
set linespace=0         " Don't insert any extra pixel lines between rows

" Highlight column 81 for old-school 80 character terminal widths
" The elseif hack is from http://stackoverflow.com/a/235970
if exists('+colorcolumn')
    set colorcolumn=81
else
    highlight ColorColumn ctermbg=235
    match ColorColumn /\%81v/
endif

" Always show signcolumn gutter for plugin diagnostics, warnings, and errors
if has("patch-8.1.1564") || has('nvim')
    " Merge signcolumn and number column into one
    set signcolumn=number
else
    set signcolumn=yes
endif

" relativenumber is a Vim 7.3 option
" If the option doesn't exist, fall back to just absolute line numbers
if exists('+relativenumber') && (version >= 704) || (version == 703) && has("patch1115")
    set relativenumber  " Display how far away each line is from the current one by default
    set number          " When used with relativenumber, the absolute line of the current
    set numberwidth=5   " Aesthetic uses only... and for displaying large line numbers

elseif exists('+relativenumber')
    set relativenumber  "Display how far away each line is from the current one by default

    " Switch to absolute line numbers whenever Vim loses focus
    autocmd FocusLost * :set number
    autocmd FocusGained * :set relativenumber

    " Use absolute line numbers when in insert mode and relative numbers when in normal mode
    autocmd InsertEnter * :set norelativenumber | set number
    autocmd InsertLeave * :set relativenumber

else
    set number          " Show absolute line numbers
endif
