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
