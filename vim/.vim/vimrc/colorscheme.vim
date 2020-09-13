" colorscheme settings

if (&t_Co == 256 || &t_Co == 88) && (!has("gui_running")) && (($TERM =~ "xterm") || ($TERM =~ "screen") || ($TERM =~ "color"))
    " Colorscheme preferences:
    " solarized, jellybeans, Tomorrow-Night, seoul256, lucius, tango2, desert, torte, murphy
    if filereadable(expand("$HOME/.vim/pack/bundle/opt/vim-colors-solarized/colors/solarized.vim"))
        colorscheme solarized
        set background=dark
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/jellybeans.vim/colors/jellybeans.vim"))
        colorscheme jellybeans
        set background=dark
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/tomorrow-theme/colors/Tomorrow-Night.vim"))
        colorscheme Tomorrow-Night
        set background=light
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/seoul256.vim/colors/seoul256.vim"))
        colorscheme seoul256
        set background=dark
    endif
endif

if has('gui_running')
    set guifont=Fira\ Code\ Retina:h16

    if filereadable(expand("$HOME/.vim/pack/bundle/opt/vim-colors-solarized/colors/solarized.vim"))
        colorscheme solarized
        set background=dark
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/jellybeans.vim/colors/jellybeans.vim"))
        colorscheme jellybeans
        set background=dark
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/tomorrow-theme/colors/Tomorrow-Night.vim"))
        colorscheme Tomorrow-Night
        set background=dark
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/seoul256.vim/colors/seoul256-light.vim"))
        colorscheme seoul256-light
        set background=dark
    endif

endif

