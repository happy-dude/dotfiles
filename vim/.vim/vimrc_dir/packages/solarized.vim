"solarized colorscheme settings

if !has("gui_running")                        " Use these settings if terminal vim; otherwise, gvimrc settings should take over
  let g:solarized_termcolors = 256            " Enable 256 color support and don't rely on terminal settings
  let g:solarized_termtrans = 0               " 1: Tell Solarized to use the default transparant terminal background; 0: Background handled by Vim
  let g:solarized_visibility = "normal"       " Set list characters can be set to one of three levels depending on your needs.
  let g:solarized_contrast = "normal"         " high/ low: Shifts some values up or down in order to expand or compress the tonal range displayed.
  "g:solarized_bold | g:solarized_underline | g:solarized_italic  " Set to 0 to turn of stylized typefaces
endif
