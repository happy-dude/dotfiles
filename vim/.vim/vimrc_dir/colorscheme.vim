" colorscheme settings

function! s:ColorschemeAvailable(name) abort
  return !empty(globpath(&runtimepath, 'colors/' . a:name . '.vim', 1))
endfunction

if (&t_Co >= 16) && (($TERM =~# "color") || ($TERM =~# "alacritty") || ($TERM =~# "wezterm") || ($TERM =~# "ghostty")) && (has("termguicolors")) && (!has("gui_running"))

  set termguicolors     " Prefer true color palettes instead of cterm 256-color palettes


  " Colorscheme preferences:
  "   gruvbox-material, solarized8_flat, gruvbox8_hard
  "   otherwise, prefer solarized w/ 256 color setting, jellybeans, Tomorrow-Night, seoul256, tango2, desert, torte, murphy

  if s:ColorschemeAvailable('gruvbox-material')
    set background=dark
    let g:gruvbox_material_palette = 'mix'
    let g:gruvbox_material_background = 'medium'
    let g:gruvbox_material_transparent_background = 1
    colorscheme gruvbox-material
  elseif s:ColorschemeAvailable('solarized8_flat')
    set background=dark
    colorscheme solarized8_flat
  elseif s:ColorschemeAvailable('gruvbox8_hard')
    set background=dark
    colorscheme gruvbox8_hard
  "elseif s:ColorschemeAvailable('base16-tomorrow-night')
  "  set termguicolors
  "  set background=light
  "  colorscheme base16-tomorrow-night
  "elseif s:ColorschemeAvailable('solarized')
  "  colorscheme solarized
  "  set background=dark
  "elseif s:ColorschemeAvailable('jellybeans')
  "  colorscheme jellybeans
  "  set background=dark
  "elseif s:ColorschemeAvailable('seoul256')
  "  colorscheme seoul256
  "  set background=dark
  endif
endif

if has('gui_running')
  set guifont=FantasqueSansM\ Nerd\ Font\ Mono:h16

  if s:ColorschemeAvailable('solarized8_flat')
    set background=dark
    colorscheme solarized8_flat
  elseif s:ColorschemeAvailable('gruvbox8_hard')
    set background=dark
    colorscheme gruvbox8_hard
  "elseif s:ColorschemeAvailable('base16-tomorrow-night')
  "  set background=dark
  "  colorscheme base16-tomorrow-night
  "elseif s:ColorschemeAvailable('solarized')
  "  colorscheme solarized
  "  set background=dark
  "elseif s:ColorschemeAvailable('jellybeans')
  "  colorscheme jellybeans
  "  set background=dark
  "elseif s:ColorschemeAvailable('seoul256-light')
  "  colorscheme seoul256-light
  "  set background=dark
  endif

endif
