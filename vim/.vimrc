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
  set esckeys           " Function keys that start with <Esc> are recognized in insert mode
endif

" vimconf directories for vimrc and plugin settings
if has('nvim')
  let s:vim_dir = '~/.config/nvim/'
  let s:vimrc_dir = '~/.config/nvim/vimrc/'
  let s:vimpack_settings_dir = '~/.config/nvim/vimrc/packages/'
else
  let s:vim_dir = '~/.vim/'
  let s:vimrc_dir = '~/.vim/vimrc/'
  let s:vimpack_settings_dir = '~/.vim/vimrc/packages/'
endif
" }


"""""""""""""""""""""""""""""""
"{ """ Plugin Configuration """
"""""""""""""""""""""""""""""""

if &loadplugins

  if has('packages')
    " { vim-plug
    packadd! vim-plug
    " }

  else
    " { pathogen fallback
    " Unconventional path to plugin (inside submodule)
    runtime pack/bundle/opt/vim-pathogen/autoload/pathogen.vim

    filetype off                " Turn filetype plugin off until after plugins are loaded

    " pathogen: Activate vim-plug
    execute pathogen#infect('pack/bundle/opt/vim-plug/')

    " pathogen: Activate plugin
    "execute pathogen#infect('pack/{}/opt/{}')
    "execute pathogen#helptags()

    syntax enable               " Enable syntax highlighting
    filetype plugin indent on   " Enable filetype plugin and filetype-based indentation
    " }

  endif

  " { Load plugin packages
  call plug#begin(s:vim_dir . 'pack/plugged/opt/')
  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'Happy-Dude/hlnext.vim'
  Plug 'cespare/vim-toml'                             ,   { 'for' : [ 'toml' ] }
  Plug 'chrisbra/csv.vim'                             ,   { 'for' : [ 'csv' ] }
  Plug 'dense-analysis/ale'
  Plug 'easymotion/vim-easymotion'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'fatih/vim-go'                                 ,   { 'for' : [ 'go', 'gomod', 'gohtmltmpl' ] }
  Plug 'gregsexton/MatchTag'                          ,   { 'for' : [ 'html', 'xml' ] }
  Plug 'guns/vim-sexp'                                ,   { 'for' : [ 'clojure', 'lisp', 'scheme', 'racket' ] }
  Plug 'justinmk/vim-dirvish'
  Plug 'keith/swift.vim'                              ,   { 'for' : [ 'swift' ] }
  Plug 'kovisoft/slimv'                               ,   { 'for' : [ 'clojure', 'lisp', 'scheme', 'racket' ] }
  Plug 'luochen1990/rainbow'
  Plug 'machakann/vim-sandwich'
  Plug 'mbbill/undotree'                              ,   { 'on' : 'UndotreeToggle' }
  Plug 'mhinz/vim-signify'
  Plug 'mileszs/ack.vim'
  Plug 'nathanaelkane/vim-indent-guides'
  Plug 'nvie/vim-flake8'                              ,   { 'for' : [ 'python' ] }
  Plug 'othree/html5.vim'                             ,   { 'for' : [ 'html', 'javascript', 'php', 'xhtml', 'xml' ] }
  Plug 'preservim/nerdcommenter'
  Plug 'rust-lang/rust.vim'                           ,   { 'for' : [ 'rust' ] }
  Plug 'sjl/clam.vim'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-markdown'                           ,   { 'for' : [ 'markdown' ] }
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-rsi'
  Plug 'tpope/vim-sexp-mappings-for-regular-people'   ,   { 'for' : [ 'clojure', 'lisp', 'scheme', 'racket' ] }
  Plug 'tpope/vim-speeddating'
  Plug 'tpope/vim-unimpaired'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'vim-latex/vim-latex'                          ,   { 'for' : [ 'tex' ] }
  Plug 'vim-pandoc/vim-pandoc'                        ,   { 'for' : [ 'markdown', 'pandoc', 'rmd', 'textile' ] }
  Plug 'vim-pandoc/vim-pandoc-syntax'                 ,   { 'for' : [ 'markdown', 'pandoc', 'rmd', 'textile' ] }
  Plug 'vim-perl/vim-perl'                            ,   { 'for' : [ 'perl', 'perl6', 'mason' ], 'branch' : 'dev' }
  Plug 'wlangstroth/vim-racket'                       ,   { 'for' : [ 'racket' ] }
  Plug 'zirrostig/vim-schlepp'

  " if fzf is available, use fzf.vim
  " otherwise, use ctrlp which is pure vimscript
  if executable('fzf')
    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'

    let s:fzf_settings    = s:vimpack_settings_dir . 'fzf.vim'
    execute 'source' s:fzf_settings

  else
    Plug 'ctrlpvim/ctrlp.vim'

    let s:ctrlp_settings  = s:vimpack_settings_dir . 'ctrlp.vim'
    execute 'source' s:ctrlp_settings
  endif

  " { Colorschemes
  Plug 'altercation/vim-colors-solarized'
  Plug 'chriskempson/base16-vim'
  Plug 'jonathanfilip/vim-lucius'
  Plug 'junegunn/seoul256.vim'
  Plug 'lifepillar/vim-gruvbox8'
  Plug 'lifepillar/vim-solarized8'
  Plug 'nanotech/jellybeans.vim'
  " }

  " { Neovim-only packages
  if has('nvim')

    " nvim-treesitter
    Plug 'nvim-treesitter/nvim-treesitter'
    Plug 'nvim-treesitter/nvim-treesitter-refactor'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'

    " Enable coc.nvim if node.js is installed
    if executable('node')
      Plug 'neoclide/coc.nvim'

      let s:coc_settings = s:vimpack_settings_dir . 'coc.vim'
      execute 'source' s:coc_settings
    endif

  endif
  " }

  call plug#end()
  " }

  " { Source plugin settings
  let s:ack_settings            = s:vimpack_settings_dir . 'ack.vim'
  let s:airline_settings        = s:vimpack_settings_dir . 'airline.vim'
  let s:ale_settings            = s:vimpack_settings_dir . 'ale.vim'
  let s:editorconfig_settings   = s:vimpack_settings_dir . 'editorconfig.vim'
  let s:dirvish_settings        = s:vimpack_settings_dir . 'dirvish.vim'
  let s:indent_guides_settings  = s:vimpack_settings_dir . 'indent_guides.vim'
  let s:latex_settings          = s:vimpack_settings_dir . 'latex.vim'
  let s:rainbow_settings        = s:vimpack_settings_dir . 'rainbow.vim'
  let s:schlepp_settings        = s:vimpack_settings_dir . 'schlepp.vim'
  let s:slimv_settings          = s:vimpack_settings_dir . 'slimv.vim'
  let s:solarized_settings      = s:vimpack_settings_dir . 'solarized.vim'

  execute 'source' s:ack_settings
  execute 'source' s:airline_settings
  execute 'source' s:ale_settings
  execute 'source' s:dirvish_settings
  execute 'source' s:editorconfig_settings
  execute 'source' s:indent_guides_settings
  execute 'source' s:latex_settings
  execute 'source' s:rainbow_settings
  execute 'source' s:schlepp_settings
  execute 'source' s:slimv_settings
  execute 'source' s:solarized_settings
  " }

endif
" }


"""""""""""""""""""""""""""""""
"{ """"       vimrc        """"
"""""""""""""""""""""""""""""""
let s:bell_settings             = s:vimrc_dir . 'bell.vim'
let s:buffer_settings           = s:vimrc_dir . 'buffer.vim'
let s:cache_settings            = s:vimrc_dir . 'cache.vim'
let s:clipboard_settings        = s:vimrc_dir . 'clipboard.vim'
let s:colorscheme_settings      = s:vimrc_dir . 'colorscheme.vim'
let s:cpoptions_settings        = s:vimrc_dir . 'cpoptions.vim'
let s:cursorlinecolumn_settings = s:vimrc_dir . 'cursorlinecolumn.vim'
let s:diff_settings             = s:vimrc_dir . 'diff.vim'
let s:encoding_settings         = s:vimrc_dir . 'encoding.vim'
let s:folding_settings          = s:vimrc_dir . 'folding.vim'
let s:formatoptions_settings    = s:vimrc_dir . 'formatoptions.vim'
let s:listchars_settings        = s:vimrc_dir . 'listchars.vim'
let s:match_settings            = s:vimrc_dir . 'match.vim'
let s:mouse_settings            = s:vimrc_dir . 'mouse.vim'
let s:search_settings           = s:vimrc_dir . 'search.vim'
let s:spellcheck_settings       = s:vimrc_dir . 'spellcheck.vim'
let s:statusline_settings       = s:vimrc_dir . 'statusline.vim'
let s:tabspaces_settings        = s:vimrc_dir . 'tabspaces.vim'
let s:visual_settings           = s:vimrc_dir . 'visual.vim'
let s:wildmenu_settings         = s:vimrc_dir . 'wildmenu.vim'
let s:wordwrap_settings         = s:vimrc_dir . 'wordwrap.vim'

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
let s:custom_mappings           = s:vimrc_dir . 'mappings.vim'
let s:custom_autocmds           = s:vimrc_dir . 'autocmds.vim'
let s:custom_functions          = s:vimrc_dir . 'functions.vim'

execute 'source' s:custom_mappings
execute 'source' s:custom_autocmds
execute 'source' s:custom_functions
" }


"""""""""""""""""""""""""""""""
"{ """" Neovim Lua Configs """"
"""""""""""""""""""""""""""""""
if has('nvim')
lua <<EOF
  require 'treesitter'
EOF
endif
" }


