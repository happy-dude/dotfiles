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
    let s:vimpack_settings_dir = '~/.config/nvim/vimrc/packages/'
    let s:vimrc_dir = '~/.config/nvim/vimrc/'
else
    let s:vimpack_settings_dir = '~/.vim/vimrc/packages/'
    let s:vimrc_dir = '~/.vim/vimrc/'
endif
" }


"""""""""""""""""""""""""""""""
"{ """ Plugin Configuration """
"""""""""""""""""""""""""""""""
filetype off            " Turn filetype plugin off until after plugins are loaded

if &loadplugins
    " { Load plugin packages
    if has('packages')
        packadd! vim-plug

        call plug#begin()
        Plug 'mileszs/ack.vim'
        Plug 'dense-analysis/ale'
        Plug 'sjl/clam.vim'
        Plug 'chrisbra/csv.vim'                             ,   { 'for' : [ 'csv' ] }
        Plug 'ctrlpvim/ctrlp.vim'
        Plug 'Happy-Dude/hlnext.vim'
        Plug 'othree/html5.vim'                             ,   { 'for' : [ 'html', 'javascript', 'php', 'xhtml', 'xml' ] }
        Plug 'gregsexton/MatchTag'                          ,   { 'for' : [ 'html', 'xml' ] }
        Plug 'preservim/nerdcommenter'
        Plug 'luochen1990/rainbow'
        Plug 'rust-lang/rust.vim'                           ,   { 'for' : [ 'rust' ] }
        Plug 'kovisoft/slimv'                               ,   { 'for' : [ 'clojure', 'lisp', 'scheme', 'racket' ] }
        Plug 'AndrewRadev/splitjoin.vim'
        Plug 'keith/swift.vim'                              ,   { 'for' : [ 'swift' ] }
        Plug 'mbbill/undotree'
        Plug 'justinmk/vim-dirvish'
        Plug 'easymotion/vim-easymotion'
        Plug 'nvie/vim-flake8'                              ,   { 'for' : [ 'python' ] }
        Plug 'tpope/vim-fugitive'
        Plug 'fatih/vim-go'                                 ,   { 'for' : [ 'go', 'gomod', 'gohtmltmpl' ] }
        Plug 'nathanaelkane/vim-indent-guides'
        Plug 'tpope/vim-markdown'                           ,   { 'for' : [ 'markdown' ] }
        Plug 'vim-latex/vim-latex'                          ,   { 'for' : [ 'tex' ] }
        Plug 'vivien/vim-linux-coding-style'                ,   { 'for' : [ 'c', 'cpp' ] }
        Plug 'vim-pandoc/vim-pandoc'                        ,   { 'for' : [ 'markdown', 'pandoc', 'rmd', 'textile' ] }
        Plug 'vim-pandoc/vim-pandoc-syntax'                 ,   { 'for' : [ 'markdown', 'pandoc', 'rmd', 'textile' ] }
        Plug 'vim-perl/vim-perl'                            ,   { 'for' : [ 'perl', 'perl6', 'mason' ], 'branch' : 'dev' }
        Plug 'wlangstroth/vim-racket'                       ,   { 'for' : [ 'racket' ] }
        Plug 'tpope/vim-repeat'
        Plug 'tpope/vim-rsi'
        Plug 'machakann/vim-sandwich'
        Plug 'zirrostig/vim-schlepp'
        Plug 'guns/vim-sexp'                                ,   { 'for' : [ 'clojure', 'lisp', 'scheme', 'racket' ] }
        Plug 'tpope/vim-sexp-mappings-for-regular-people'   ,   { 'for' : [ 'clojure', 'lisp', 'scheme', 'racket' ] }
        Plug 'tpope/vim-speeddating'
        Plug 'cespare/vim-toml'                             ,   { 'for' : [ 'toml' ] }
        Plug 'tpope/vim-unimpaired'

        " { Colorschemes
        Plug 'nanotech/jellybeans.vim'
        Plug 'junegunn/seoul256.vim'
        Plug 'chriskempson/base16-vim'
        Plug 'altercation/vim-colors-solarized'
        Plug 'jonathanfilip/vim-lucius'
        Plug 'lifepillar/vim-solarized8'
        Plug 'lifepillar/vim-gruvbox8'
        " }

        " { Neovim-only packages
        if has('nvim')

            " Enable coc.nvim if node.js is installed
            if executable('node')
                Plug 'neoclide/coc.nvim'

                let s:coc_settings = s:vimpack_settings_dir . 'coc.vim'
                execute 'source' s:coc_settings
            endif

        endif
        call plug#end()
        " }

    else
        " { pathogen fallback
        " Unconventional path to plugin (inside submodule)
        " for when packadd! is unavailable
        runtime pack/bundle/opt/vim-pathogen/autoload/pathogen.vim
        "
        " pathogen: Activate plugin
        call pathogen#infect('plugged/{}')
        call pathogen#helptags()
        " }
    endif
    " }

    " { Source vim plugin settings
    let s:ack_settings              = s:vimpack_settings_dir . 'ack.vim'
    let s:ale_settings              = s:vimpack_settings_dir . 'ale.vim'
    let s:ctrlp_settings            = s:vimpack_settings_dir . 'ctrlp.vim'
    let s:dirvish_settings          = s:vimpack_settings_dir . 'dirvish.vim'
    let s:indent_guides_settings    = s:vimpack_settings_dir . 'indent_guides.vim'
    let s:latex_settings            = s:vimpack_settings_dir . 'latex.vim'
    let s:rainbow_settings          = s:vimpack_settings_dir . 'rainbow.vim'
    let s:schlepp_settings          = s:vimpack_settings_dir . 'schlepp.vim'
    let s:slimv_settings            = s:vimpack_settings_dir . 'slimv.vim'
    let s:solarized_settings        = s:vimpack_settings_dir . 'solarized.vim'

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
let s:bell_settings                 = s:vimrc_dir . 'bell.vim'
let s:buffer_settings               = s:vimrc_dir . 'buffer.vim'
let s:cache_settings                = s:vimrc_dir . 'cache.vim'
let s:clipboard_settings            = s:vimrc_dir . 'clipboard.vim'
let s:colorscheme_settings          = s:vimrc_dir . 'colorscheme.vim'
let s:cpoptions_settings            = s:vimrc_dir . 'cpoptions.vim'
let s:cursorlinecolumn_settings     = s:vimrc_dir . 'cursorlinecolumn.vim'
let s:diff_settings                 = s:vimrc_dir . 'diff.vim'
let s:encoding_settings             = s:vimrc_dir . 'encoding.vim'
let s:folding_settings              = s:vimrc_dir . 'folding.vim'
let s:formatoptions_settings        = s:vimrc_dir . 'formatoptions.vim'
let s:listchars_settings            = s:vimrc_dir . 'listchars.vim'
let s:match_settings                = s:vimrc_dir . 'match.vim'
let s:mouse_settings                = s:vimrc_dir . 'mouse.vim'
let s:search_settings               = s:vimrc_dir . 'search.vim'
let s:spellcheck_settings           = s:vimrc_dir . 'spellcheck.vim'
let s:statusline_settings           = s:vimrc_dir . 'statusline.vim'
let s:tabspaces_settings            = s:vimrc_dir . 'tabspaces.vim'
let s:visual_settings               = s:vimrc_dir . 'visual.vim'
let s:wildmenu_settings             = s:vimrc_dir . 'wildmenu.vim'
let s:wordwrap_settings             = s:vimrc_dir . 'wordwrap.vim'

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
let s:custom_mappings               = s:vimrc_dir . 'mappings.vim'
let s:custom_autocmds               = s:vimrc_dir . 'autocmds.vim'
let s:custom_functions              = s:vimrc_dir . 'functions.vim'

execute 'source' s:custom_mappings
execute 'source' s:custom_autocmds
execute 'source' s:custom_functions
" }

