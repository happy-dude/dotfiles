" colorscheme settings

" vimconf directories for vimrc and plugin settings
if has('nvim')
  let s:vim_plugged_dir = '~/.config/nvim/pack/plugged/opt/'
else
  let s:vim_plugged_dir = '~/.vim/pack/plugged/opt/'
endif

if (&t_Co >= 16) && (($TERM =~# "color") || ($TERM =~# "alacritty")) && (has("termguicolors")) && (!has("gui_running"))

  set notermguicolors     " Personally prefer the flattened 256-color palettes instead of true color ones

  " Colorscheme preferences:
  "   solarized8_flat, gruvbox8_hard
  "   otherwise, prefer solarized w/ 256 color setting, jellybeans, Tomorrow-Night, seoul256, lucius, tango2, desert, torte, murphy

  if filereadable(expand(s:vim_plugged_dir . "/vim-solarized8/colors/solarized8_flat.vim"))
    set background=dark
    colorscheme solarized8_flat
  elseif filereadable(expand(s:vim_plugged_dir . "/vim-gruvbox8/colors/gruvbox8_hard.vim"))
    set background=dark
    colorscheme gruvbox8_hard
  "elseif filereadable(expand(s:vim_plugged_dir . "/base16-vim/colors/base16-tomorrow-night.vim"))
  "  set termguicolors
  "  set background=light
  "  colorscheme base16-tomorrow-night
  "elseif filereadable(expand(s:vim_plugged_dir . "/vim-colors-solarized/colors/solarized.vim"))
  "  colorscheme solarized
  "  set background=dark
  "elseif filereadable(expand(s:vim_plugged_dir . "/jellybeans.vim/colors/jellybeans.vim"))
  "  colorscheme jellybeans
  "  set background=dark
  "elseif filereadable(expand(s:vim_plugged_dir . "/seoul256.vim/colors/seoul256.vim"))
  "  colorscheme seoul256
  "  set background=dark
  endif
endif

if has('gui_running')
  set guifont=Fira\ Code\ Retina:h16

  if filereadable(expand(s:vim_plugged_dir . "/vim-solarized8/colors/solarized8_flat.vim"))
    set background=dark
    colorscheme solarized8_flat
  elseif filereadable(expand(s:vim_plugged_dir . "/vim-gruvbox8/colors/gruvbox8_hard.vim"))
    set background=dark
    colorscheme gruvbox8_hard
  "elseif filereadable(expand(s:vim_plugged_dir . "/base16-vim/colors/base16-tomorrow-night.vim"))
  "  set background=dark
  "  colorscheme base16-tomorrow-night
  "elseif filereadable(expand(s:vim_plugged_dir . "/vim-colors-solarized/colors/solarized.vim"))
  "  colorscheme solarized
  "  set background=dark
  "elseif filereadable(expand(s:vim_plugged_dir . "/jellybeans.vim/colors/jellybeans.vim"))
  "  colorscheme jellybeans
  "  set background=dark
  "elseif filereadable(expand(s:vim_plugged_dir . "/seoul256.vim/colors/seoul256-light.vim"))
  "  colorscheme seoul256-light
  "  set background=dark
  endif

endif
