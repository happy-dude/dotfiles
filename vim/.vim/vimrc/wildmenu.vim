" wildmenu settings

set wildmenu                    " Turn on wild menu
set wildmode=list:longest,full  " Turn on wild mode huge list
set wildchar=<TAB>              " Path/file expansion in colon mode

" Ignore this list of file extensions
" */ for ctrlp globbing
set wildignore=*/.bak
set wildignore+=*/.sw?                                  " Vim swap files
set wildignore+=*/.DS_Store                             " OS X bullshit
set wildignore+=*/.spl                                  " Compiled spelling word lists
set wildignore+=*/.bmp,*/.gif,*/.jpg,*/.jpeg,*/.png     " Binary images
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*               " Version control
set wildignore+=*/.orig                                 " Merge resolution files
set wildignore+=*/.dll,*/.exe,*/.o,*/.obj,*/.manifest   " Compiled object files
set wildignore+=*/.pyc                                  " Python byte code
set wildignore+=migrations                              " Django migrations
set wildignore+=*/.luac                                 " Lua byte code
set wildignore+=*/.aux,*/.out,*/.toc                    " LaTeX intermediate files

set title               " Show title in console title bar
set guioptions=em       " e ensures tab bar is displayed in GUI; m ensures menu bar is displayed
set guitablabel=%t      " Display buffer name on tab label
set showcmd             " Display commands as they are typed
"set cmdheight=1         " Set the commandbar height -- default
set showtabline=2       " Display tab line even if single file opened
"set nohidden            " When I close a tab, remove from buffer
set tm=500              " Set tooltip menu
