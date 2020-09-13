"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""         vimrc Configuration
"""
"""     Author: Stanley Chan
"""     Github: https://github.com/Happy-Dude
"""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
