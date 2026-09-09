"solarized colorscheme settings

if !has("gui_running")                        " Use these settings if terminal vim; otherwise, gvimrc settings should take over
  let g:solarized_termtrans = 0               " 1: Tell Solarized to use the default transparant terminal background; 0: Background handled by Vim
  let g:solarized_visibility = "normal"       " Set list characters can be set to one of three levels depending on your needs.
  "g:solarized_bold | g:solarized_underline | g:solarized_italic  " Set to 0 to turn of stylized typefaces
endif
