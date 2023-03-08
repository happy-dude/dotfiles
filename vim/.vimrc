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

  filetype off                " Turn filetype plugin off until after plugins are loaded

  if has('packages')
    " { vim-plug
    packadd! vim-plug
    " }

  else
    " { pathogen fallback
    " Unconventional path to plugin (inside submodule)
    runtime pack/bundle/opt/vim-pathogen/autoload/pathogen.vim


    " pathogen: Activate vim-plug
    execute pathogen#infect('pack/bundle/opt/vim-plug/')

    " pathogen: Activate plugin
    "execute pathogen#infect('pack/{}/opt/{}')
    "execute pathogen#helptags()

    " }

  endif

  " { Load plugin packages
  call plug#begin(s:vim_dir . 'pack/plugged/opt/')
  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'Glench/Vim-Jinja2-Syntax'
  Plug 'Happy-Dude/hlnext.vim'
  Plug 'cespare/vim-toml'                             ,   { 'for' : [ 'toml' ] }
  Plug 'chrisbra/csv.vim'                             ,   { 'for' : [ 'csv' ] }
  Plug 'dense-analysis/ale'
  Plug 'easymotion/vim-easymotion'                    ,   !has('nvim') ? {} : { 'on': [] }
  Plug 'editorconfig/editorconfig-vim'
  Plug 'fatih/vim-go'                                 ,   { 'for' : [ 'go', 'gomod', 'gohtmltmpl' ] }
  Plug 'gregsexton/MatchTag'                          ,   { 'for' : [ 'html', 'xml' ] }
  Plug 'guns/vim-sexp'                                ,   { 'for' : [ 'clojure', 'fennel', 'lisp', 'scheme', 'racket' ] }
  Plug 'justinmk/vim-dirvish'
  Plug 'keith/swift.vim'                              ,   { 'for' : [ 'swift' ] }
  Plug 'kovisoft/slimv'                               ,   { 'for' : [ 'clojure', 'fennel', 'lisp', 'scheme', 'racket' ] }
  Plug 'lervag/vimtex'                                ,   { 'for' : [ 'tex' ] }
  Plug 'luochen1990/rainbow'                          ,   !has('nvim') ? {} : { 'on': [] }
  Plug 'machakann/vim-sandwich'
  Plug 'mbbill/undotree'                              ,   { 'on' : 'UndotreeToggle' }
  Plug 'mhinz/vim-signify'
  Plug 'mileszs/ack.vim'
  Plug 'nathanaelkane/vim-indent-guides'
  Plug 'nvie/vim-flake8'                              ,   { 'for' : [ 'python' ] }
  Plug 'othree/html5.vim'                             ,   { 'for' : [ 'html', 'javascript', 'php', 'xhtml', 'xml' ] }
  Plug 'preservim/nerdcommenter'
  Plug 'rust-lang/rust.vim'                           ,   { 'for' : [ 'rust' ] }
  Plug 'saltstack/salt-vim'
  Plug 'sjl/clam.vim'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-markdown'                           ,   { 'for' : [ 'markdown' ] }
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-rsi'
  Plug 'tpope/vim-sexp-mappings-for-regular-people'   ,   { 'for' : [ 'clojure', 'fennel', 'lisp', 'scheme', 'racket' ] }
  Plug 'tpope/vim-speeddating'
  Plug 'tpope/vim-unimpaired'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'vim-pandoc/vim-pandoc'                        ,   { 'for' : [ 'markdown', 'pandoc', 'rmd', 'textile' ] }
  Plug 'vim-pandoc/vim-pandoc-syntax'                 ,   { 'for' : [ 'markdown', 'pandoc', 'rmd', 'textile' ] }
  Plug 'vim-perl/vim-perl'                            ,   { 'for' : [ 'perl', 'perl6', 'mason' ], 'branch' : 'dev', 'do': 'make clean carp dancer highlight-all-pragmas moose test-more try-tiny' }
  Plug 'wlangstroth/vim-racket'                       ,   { 'for' : [ 'racket' ] }
  "Plug 'zirrostig/vim-schlepp'

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
  Plug 'sainnhe/gruvbox-material'
  " }

  " { Neovim-only packages
  if has('nvim')

    " hop.nvim
    Plug 'phaazon/hop.nvim'                               " vim-easymotion
    "
    " orgmode.nvim
    Plug 'kristijanhusak/orgmode.nvim'

    " which-key.nvim
    Plug 'folke/which-key.nvim'

    " Lisp + neovim-lua-based REPL
    Plug 'Olical/aniseed'                             ,   { 'for' : [ 'fennel' ] }
    Plug 'Olical/conjure'                             ,   { 'for' : [ 'clojure', 'fennel', 'lisp', 'scheme', 'racket' ] }
    "Plug 'gpanders/nvim-parinfer'

    " nvim-dap
    Plug 'mfussenegger/nvim-dap'
    Plug 'rcarriga/nvim-dap-ui'

    " nvim-treesitter
    Plug 'nvim-treesitter/nvim-treesitter'
    Plug 'nvim-treesitter/nvim-treesitter-refactor'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'https://gitlab.com/HiPhish/nvim-ts-rainbow2.git'  " luochen1990/rainbow

    " neogit
    Plug 'nvim-lua/plenary.nvim'
    Plug 'TimUntersberger/neogit'

    " Enable coc.nvim if node.js is installed
    if executable('node')
      Plug 'neoclide/coc.nvim'                        ,   {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}

      let s:coc_settings = s:vimpack_settings_dir . 'coc.vim'
      execute 'source' s:coc_settings
    endif

  endif
  " }

  call plug#end()
  " }

  " { Source plugin settings
  let s:ack_settings            = s:vimpack_settings_dir . 'ack.vim'
  execute                       'source' s:ack_settings

  let s:airline_settings        = s:vimpack_settings_dir . 'airline.vim'
  execute                       'source' s:airline_settings

  let s:ale_settings            = s:vimpack_settings_dir . 'ale.vim'
  execute                       'source' s:ale_settings

  let s:dirvish_settings        = s:vimpack_settings_dir . 'dirvish.vim'
  execute                       'source' s:dirvish_settings

  let s:editorconfig_settings   = s:vimpack_settings_dir . 'editorconfig.vim'
  execute                       'source' s:editorconfig_settings

  let s:indent_guides_settings  = s:vimpack_settings_dir . 'indent_guides.vim'
  execute                       'source' s:indent_guides_settings

  if !has('nvim')               " disable rainbow in vim
    let s:rainbow_settings      = s:vimpack_settings_dir . 'rainbow.vim'
    execute                     'source' s:rainbow_settings
  endif

  "let s:schlepp_settings        = s:vimpack_settings_dir . 'schlepp.vim'
  "execute                       'source' s:schlepp_settings

  let s:slimv_settings          = s:vimpack_settings_dir . 'slimv.vim'
  execute                       'source' s:slimv_settings

  let s:solarized_settings      = s:vimpack_settings_dir . 'solarized.vim'
  execute                       'source' s:solarized_settings

  let s:vimtex_settings         = s:vimpack_settings_dir . 'vimtex.vim'
  execute                       'source' s:vimtex_settings

  " }

  syntax enable               " Enable syntax highlighting
  filetype plugin indent on   " Enable filetype plugin and filetype-based indentation

endif
" }


"""""""""""""""""""""""""""""""
"{ """"       vimrc        """"
"""""""""""""""""""""""""""""""
let s:bell_settings             = s:vimrc_dir . 'bell.vim'
execute                         'source' s:bell_settings

let s:buffer_settings           = s:vimrc_dir . 'buffer.vim'
execute                         'source' s:buffer_settings

let s:cache_settings            = s:vimrc_dir . 'cache.vim'
execute                         'source' s:cache_settings

let s:clipboard_settings        = s:vimrc_dir . 'clipboard.vim'
execute                         'source' s:clipboard_settings

let s:colorscheme_settings      = s:vimrc_dir . 'colorscheme.vim'
execute                         'source' s:colorscheme_settings

let s:cpoptions_settings        = s:vimrc_dir . 'cpoptions.vim'
execute                         'source' s:cpoptions_settings

let s:cursorlinecolumn_settings = s:vimrc_dir . 'cursorlinecolumn.vim'
execute                         'source' s:cursorlinecolumn_settings

let s:diff_settings             = s:vimrc_dir . 'diff.vim'
execute                         'source' s:diff_settings

let s:encoding_settings         = s:vimrc_dir . 'encoding.vim'
execute                         'source' s:encoding_settings

let s:folding_settings          = s:vimrc_dir . 'folding.vim'
execute                         'source' s:folding_settings

let s:formatoptions_settings    = s:vimrc_dir . 'formatoptions.vim'
execute                         'source' s:formatoptions_settings

let s:listchars_settings        = s:vimrc_dir . 'listchars.vim'
execute                         'source' s:listchars_settings

let s:match_settings            = s:vimrc_dir . 'match.vim'
execute                         'source' s:match_settings

let s:mouse_settings            = s:vimrc_dir . 'mouse.vim'
execute                         'source' s:mouse_settings

let s:search_settings           = s:vimrc_dir . 'search.vim'
execute                         'source' s:search_settings

let s:spellcheck_settings       = s:vimrc_dir . 'spellcheck.vim'
execute                         'source' s:spellcheck_settings

let s:statusline_settings       = s:vimrc_dir . 'statusline.vim'
execute                         'source' s:statusline_settings

let s:tabspaces_settings        = s:vimrc_dir . 'tabspaces.vim'
execute                         'source' s:tabspaces_settings

let s:visual_settings           = s:vimrc_dir . 'visual.vim'
execute                         'source' s:visual_settings

let s:wildmenu_settings         = s:vimrc_dir . 'wildmenu.vim'
execute                         'source' s:wildmenu_settings

let s:wordwrap_settings         = s:vimrc_dir . 'wordwrap.vim'
execute                         'source' s:wordwrap_settings
" }


"""""""""""""""""""""""""""""""
"{ """"   Custom Configs   """"
"""""""""""""""""""""""""""""""
let s:custom_mappings           = s:vimrc_dir . 'mappings.vim'
execute                         'source' s:custom_mappings

let s:custom_autocmds           = s:vimrc_dir . 'autocmds.vim'
execute                         'source' s:custom_autocmds

let s:custom_functions          = s:vimrc_dir . 'functions.vim'
execute                         'source' s:custom_functions
" }


"""""""""""""""""""""""""""""""
"{ """" Neovim Lua Configs """"
"""""""""""""""""""""""""""""""
if has('nvim')
lua <<EOF
  -- configs in lua/ directory
  require('qol')

  require('hop_nvim')
  require('org')
  require('treesitter')

  require('neogit').setup{}
  require('which-key').setup{}
EOF
endif
" }