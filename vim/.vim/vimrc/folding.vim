" folding settings

set foldenable          " Turn on folding
set foldmarker={,}      " Fold C-style code (only user this as default if you use a high foldlevel
set foldmethod=marker   " Fold on the marker
set foldlevel=100       " Don't autofold anything (can still fold manually)
set foldopen=block,hor,mark,percent,quickfix,tag    " Define movement that opens folds
