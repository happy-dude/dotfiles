" settings related to visuals, scrolling, drawing, etc.

"set ttyscroll=0         " Number of lines to scroll; 0 for terminals where scrolling is slow and redrawing is fast
"set ttyfast             " Fast terminal connection; more chars will be sent to screen for redrawing
"set scrolloff=0        " Set 'x' lines to the cursors position - when moving verticle
"set sidescrolloff=0    " Keep at least 'x' lines left/right
set lazyredraw          " Do not redraw while running macros; buffers screen updates
set more                " Use more prompt -- pausing screen when too many items are displayed

set title               " Show title in console title bar
set showtabline=2       " Display tab line even if single file opened
set tm=500              " Set tooltip menu

set guioptions=em       " e ensures tab bar is displayed in GUI; m ensures menu bar is displayed
set guitablabel=%t      " Display buffer name on tab label

set showcmd             " Display commands as they are typed
"set cmdheight=1         " Set the commandbar height -- default

set updatetime=100      " Default 4000ms (4s); quicker updatetime for responsive async plugins like signify and coc
