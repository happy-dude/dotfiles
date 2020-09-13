" spaces over tabs

"set cindent             " No need to set cindent since it is automatic for C-based files
"set smartindent         " Smartindent: auto inserts one extra level of indentation for C-like files.
set autoindent          " Autoindent: auto indents a new line to the same indentation used by previous line

set expandtab           " Default tabs into spaces
set shiftwidth=4        " Auto-indent amount when using cindent, >>, << and stuff like that
set softtabstop=4       " When hitting a tab or backspace- spaces should a tab should be
set tabstop=8           " Vim's default tabstop is 8
set smarttab            " A <Tab> in front of a line inserts blanks according to settings; <BS> deletes a shiftwidth
set shiftround          " When at 3 spaces and hit > ... go to 4, not 5
