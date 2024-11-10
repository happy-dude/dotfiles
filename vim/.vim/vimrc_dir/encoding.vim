" file encoding settings

" Enable Vim-specific, non-Neovim features
if !has('nvim')
  set encoding=utf-8              " Default to UTF-8 encoding
  set termencoding=utf-8
endif

set fileencoding=utf-8
set fileformats=unix,dos,mac    " Default file formats
