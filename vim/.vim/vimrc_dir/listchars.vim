" list chars settings

set list                " List mode: show special characters -- what is displayed is changed by listchars
                        " Inspired by https://old.reddit.com/r/vim/comments/4hoa6e/what_do_you_use_for_your_listchars/
set listchars=tab:⋮\ ,trail:·,nbsp:∘
set listchars+=extends:›,precedes:‹
"set listchars+=eol:¬    " or eol:$ or eol:↲

if v:version > 704 || v:version == 704 && has("patch338")
  set breakindent showbreak=↪\   " Indent wrapped lines and mark them with an arrow
  "set breakindent showbreak=..\ 
  "set breakindent showbreak=\\ 
  "set breakindent showbreak=…\ 
  "set breakindent showbreak=⋯\ 
endif
