"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""         vimrc Configuration
"""
"""     Author: Stanley Chan
"""     Github: https://github.com/Happy-Dude
"""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""
"{ """      Preamble       """"
"""""""""""""""""""""""""""""""
set nocompatible        " Use Vim defaults
set modelines=0         " Set modelines to 0 to prevent several security exploits

" Enable Vim-specific, non-Neovim features
if !has('nvim')
    set esckeys         " Function keys that start with <Esc> are recognized in insert mode
endif

" vimconf directories for vimrc and plugin settings
if has('nvim')
    let s:vimpackages = '~/.config/nvim/vimrc/packages/'
    let s:vimdir = '~/.config/nvim/vimrc/'
else
    let s:vimpackages = '~/.vim/vimrc/packages/'
    let s:vimdir = '~/.vim/vimrc/'
endif
" }


"""""""""""""""""""""""""""""""
"{ """ Plugin Configuration """
"""""""""""""""""""""""""""""""
filetype off            " Turn filetype plugin off until Pathogen loads

if &loadplugins
    " { Load plugin packages
    if has('packages')
        packadd! ack.vim
        packadd! ale
        packadd! clam.vim
        packadd! csv.vim
        packadd! ctrlp.vim
        packadd! hlnext.vim
        packadd! html5.vim
        packadd! MatchTag
        packadd! nerdcommenter
        packadd! rainbow
        packadd! rust.vim
        packadd! slimv
        packadd! splitjoin.vim
        packadd! swift.vim
        packadd! undotree
        packadd! vim-dirvish
        packadd! vim-easymotion
        packadd! vim-flake8
        packadd! vim-fugitive
        packadd! vim-go
        packadd! vim-indent-guides
        packadd! vim-latex
        packadd! vim-linux-coding-style
        packadd! vim-markdown
        packadd! vim-pandoc
        packadd! vim-pandoc-syntax
        packadd! vim-perl
        packadd! vim-racket
        packadd! vim-repeat
        packadd! vim-rsi
        packadd! vim-sandwich
        packadd! vim-schlepp
        packadd! vim-sexp
        packadd! vim-sexp-mappings-for-regular-people
        packadd! vim-speeddating
        packadd! vim-toml
        packadd! vim-unimpaired

        " { Neovim-only packages
        if has('nvim')

            " Enable coc.nvim if node.js is installed
            if executable('node')
                packadd! coc.nvim

                let s:coc_settings = s:vimpackages . 'coc.vim'
                execute 'source' s:coc_settings
            endif

        endif
        " }

        " { Colorschemes
        " colorschemes are automatically discovered
        " packadd! jellybeans.vim
        " packadd! seoul256.vim
        " packadd! tomorrow-theme
        " packadd! vim-colors-solarized
        " packadd! vim-lucius
        " }
    else
        " { pathogen fallback
        " Unconventional path to plugin (inside submodule)
        runtime pack/bundle/opt/vim-pathogen/autoload/pathogen.vim

        " pathogen: Activate plugin
        call pathogen#infect('pack/bundle/opt/{}')
        call pathogen#helptags()
        " }
    endif
    " }

    " { Source vim plugin settings
    let s:ack_settings              = s:vimpackages . 'ack.vim'
    let s:ale_settings              = s:vimpackages . 'ale.vim'
    let s:ctrlp_settings            = s:vimpackages . 'ctrlp.vim'
    let s:dirvish_settings          = s:vimpackages . 'dirvish.vim'
    let s:indent_guides_settings    = s:vimpackages . 'indent_guides.vim'
    let s:latex_settings            = s:vimpackages . 'latex.vim'
    let s:rainbow_settings          = s:vimpackages . 'rainbow.vim'
    let s:schlepp_settings          = s:vimpackages . 'schlepp.vim'
    let s:slimv_settings            = s:vimpackages . 'slimv.vim'
    let s:solarized_settings        = s:vimpackages . 'solarized.vim'

    execute 'source' s:ack_settings
    execute 'source' s:ale_settings
    execute 'source' s:ctrlp_settings
    execute 'source' s:dirvish_settings
    execute 'source' s:indent_guides_settings
    execute 'source' s:latex_settings
    execute 'source' s:rainbow_settings
    execute 'source' s:schlepp_settings
    execute 'source' s:slimv_settings
    execute 'source' s:solarized_settings
    " }

endif

syntax enable               " Enable syntax highlighting
filetype plugin indent on   " Enable filetype plugin and filetype-based indentation
silent! helptags ALL        " Generate documentation tages automatically
" }


"""""""""""""""""""""""""""""""
"{ """"       vimrc        """"
"""""""""""""""""""""""""""""""
let s:bell_settings                 = s:vimdir . 'bell.vim'
let s:buffer_settings               = s:vimdir . 'buffer.vim'
let s:cache_settings                = s:vimdir . 'cache.vim'
let s:clipboard_settings            = s:vimdir . 'clipboard.vim'
let s:colorscheme_settings          = s:vimdir . 'colorscheme.vim'
let s:cpoptions_settings            = s:vimdir . 'cpoptions.vim'
let s:cursorlinecolumn_settings     = s:vimdir . 'cursorlinecolumn.vim'
let s:diff_settings                 = s:vimdir . 'diff.vim'
let s:encoding_settings             = s:vimdir . 'encoding.vim'
let s:folding_settings              = s:vimdir . 'folding.vim'
let s:formatoptions_settings        = s:vimdir . 'formatoptions.vim'
let s:listchars_settings            = s:vimdir . 'listchars.vim'
let s:match_settings                = s:vimdir . 'match.vim'
let s:mouse_settings                = s:vimdir . 'mouse.vim'
let s:search_settings               = s:vimdir . 'search.vim'
let s:spellcheck_settings           = s:vimdir . 'spellcheck.vim'
let s:statusline_settings           = s:vimdir . 'statusline.vim'
let s:tabspaces_settings            = s:vimdir . 'tabspaces.vim'
let s:visual_settings               = s:vimdir . 'visual.vim'
let s:wildmenu_settings             = s:vimdir . 'wildmenu.vim'
let s:wordwrap_settings             = s:vimdir . 'wordwrap.vim'

execute 'source' s:bell_settings
execute 'source' s:buffer_settings
execute 'source' s:cache_settings
execute 'source' s:clipboard_settings
execute 'source' s:colorscheme_settings
execute 'source' s:cpoptions_settings
execute 'source' s:cursorlinecolumn_settings
execute 'source' s:diff_settings
execute 'source' s:encoding_settings
execute 'source' s:folding_settings
execute 'source' s:formatoptions_settings
execute 'source' s:listchars_settings
execute 'source' s:match_settings
execute 'source' s:mouse_settings
execute 'source' s:search_settings
execute 'source' s:spellcheck_settings
execute 'source' s:statusline_settings
execute 'source' s:tabspaces_settings
execute 'source' s:visual_settings
execute 'source' s:wildmenu_settings
execute 'source' s:wordwrap_settings
" }


"""""""""""""""""""""""""""""""
"{ """"   Custom Configs   """"
"""""""""""""""""""""""""""""""
let s:custom_mappings               = s:vimdir . 'mappings.vim'
let s:custom_autocmds               = s:vimdir . 'autocmds.vim'
let s:custom_functions              = s:vimdir . 'functions.vim'

execute 'source' s:custom_mappings
execute 'source' s:custom_autocmds
execute 'source' s:custom_functions
" }

