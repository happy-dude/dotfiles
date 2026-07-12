" colorscheme settings

function! s:ColorschemeAvailable(name) abort
  return !empty(globpath(&runtimepath, 'colors/' . a:name . '.vim', 1))
endfunction

if (&t_Co >= 16) && (($TERM =~# "color") || ($TERM =~# "alacritty") || ($TERM =~# "wezterm") || ($TERM =~# "ghostty")) && (has("termguicolors")) && (!has("gui_running"))

  set termguicolors     " Prefer true color palettes instead of cterm 256-color palettes


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
  endif

endif
