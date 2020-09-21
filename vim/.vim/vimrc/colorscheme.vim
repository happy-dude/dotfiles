" colorscheme settings

if (&t_Co >= 16) && (($TERM =~# "color") || ($TERM =~# "alacritty")) && (has("termguicolors")) && (!has("gui_running"))

    set notermguicolors     " Personally prefer the flattened 256-color palettes instead of true color ones

    " Colorscheme preferences:
    "   solarized8_flat, gruvbox8_hard
    "   otherwise, prefer solarized w/ 256 color setting, jellybeans, Tomorrow-Night, seoul256, lucius, tango2, desert, torte, murphy

    if filereadable(expand("$HOME/.vim/plugged/vim-solarized8/colors/solarized8_flat.vim"))
        set background=dark
        colorscheme solarized8_flat
    elseif filereadable(expand("$HOME/.vim/plugged/vim-gruvbox8/colors/gruvbox8_hard.vim"))
        set background=dark
        colorscheme gruvbox8_hard
    "elseif filereadable(expand("$HOME/.vim/plugged/vim-colors-solarized/colors/solarized.vim"))
    "    colorscheme solarized
    "    set background=dark
    "elseif filereadable(expand("$HOME/.vim/plugged/jellybeans.vim/colors/jellybeans.vim"))
    "    colorscheme jellybeans
    "    set background=dark
    "elseif filereadable(expand("$HOME/.vim/plugged/base16-vim/colors/base16-tomorrow-night.vim"))
    "    set termguicolors
    "    set background=light
    "    colorscheme base16-tomorrow-night
    "elseif filereadable(expand("$HOME/.vim/plugged/seoul256.vim/colors/seoul256.vim"))
    "    colorscheme seoul256
    "    set background=dark
    endif
endif

if has('gui_running')
    set guifont=Fira\ Code\ Retina:h16

    if filereadable(expand("$HOME/.vim/plugged/vim-solarized8/colors/solarized8_flat.vim"))
        set background=dark
        colorscheme solarized8_flat
    elseif filereadable(expand("$HOME/.vim/plugged/vim-gruvbox8/colors/gruvbox8_hard.vim"))
        set background=dark
        colorscheme gruvbox8_hard
    "elseif filereadable(expand("$HOME/.vim/plugged/vim-colors-solarized/colors/solarized.vim"))
    "    colorscheme solarized
    "    set background=dark
    "elseif filereadable(expand("$HOME/.vim/plugged/jellybeans.vim/colors/jellybeans.vim"))
    "    colorscheme jellybeans
    "    set background=dark
    "elseif filereadable(expand("$HOME/.vim/plugged/base16-vim/colors/base16-tomorrow-night.vim"))
    "    set background=dark
    "    colorscheme base16-tomorrow-night
    "elseif filereadable(expand("$HOME/.vim/plugged/seoul256.vim/colors/seoul256-light.vim"))
    "    colorscheme seoul256-light
    "    set background=dark
    endif

endif
