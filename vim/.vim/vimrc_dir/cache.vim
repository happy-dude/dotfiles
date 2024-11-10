" cache directory settings

set backup              " Keep backup file after overwriting a file
set writebackup         " Make a backup file before overwriting a file
set swapfile            " Allow creation of .swp swap files
if exists('+undofile')  " Have Vim save undo history to file and restore it on open
  set undofile
endif
"Default history and undolevels = 1000
"set history=350         " Sets how many lines of history Vim will remember
"set undolevels=500      " Number of undo levels

" Cache things to our home .cache dir
if isdirectory(expand('$HOME/.cache/vim')) && isdirectory(expand('$HOME/.local/share/nvim'))
  if !has('nvim')       " Neovim defaults use $XDG_DATA_HOME/nvim
                        " i.e. ~/.local/share/nvim
    if &backupdir =~# '^\.,'
      set backupdir^=$HOME/.cache/vim/backup//
    endif

    if &directory =~# '^\.,'
      set directory^=$HOME/.cache/vim/swap//
    endif

    if exists('+undodir') && &undodir =~# '^\.\%(,\|$\)'
      set undodir^=$HOME/.cache/vim/undo//
    endif

    if exists('+viewdir')
      set viewdir=$HOME/.cache/vim/view//
    endif

  elseif has('nvim')    " Neovim backup default has local '.' precedence
                        " override to ~/.local/share/nvim/backup
    if &backupdir =~# '^\.,'
      set backupdir=$HOME/.local/share/nvim/backup//
    endif

  endif
else

  if !has('nvim')
    call mkdir(expand("$HOME/.cache/vim"), "p")
    call mkdir(expand("$HOME/.cache/vim/backup"), "p")
    call mkdir(expand("$HOME/.cache/vim/swap"), "p")
    call mkdir(expand("$HOME/.cache/vim/undo"), "p")
    call mkdir(expand("$HOME/.cache/vim/view"), "p")

    set backupdir^=$HOME/.cache/vim/backup//
    set directory^=$HOME/.cache/vim/swap//
    set undodir^=$HOME/.cache/vim/undo//
    set viewdir=$HOME/.cache/vim/view//

  elseif has('nvim')
    call mkdir(expand("$HOME/.local/share/nvim/backup"), "p")
    set backupdir=$HOME/.local/share/nvim/backup//

  endif
endif
