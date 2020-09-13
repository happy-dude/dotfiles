"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""         vimrc Configuration
"""
"""     Author: Stanley Chan
"""     Github: https://github.com/Happy-Dude
"""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set nocompatible        " Use Vim defaults
set modelines=0         " Set modelines to 0 to prevent several security exploits

" Enable Vim-specific, non-Neovim features
if !has('nvim')
    set esckeys         " Function keys that start with <Esc> are recognized in insert mode
endif

" Enable mouse support in console
if has('mouse')
    set mouse=a
    if !has('nvim')
        if ($TERM =~ "xterm" || $TERM =~ "screen" || $TERM =~ "urxvt")
            " for some reason, doing this directly with 'set ttymouse=xterm2'
            " doesn't work -- 'set ttymouse?' returns xterm2 but the mouse
            " makes tmux enter copy mode instead of selecting or scrolling
            " inside Vim -- but luckily, setting it up from within autocmds
            " works
            autocmd VimEnter * set ttymouse=xterm2
            autocmd FocusGained * set ttymouse=xterm2
            autocmd BufEnter * set ttymouse=xterm2
        endif
    endif
endif

""""""""""""""""""""""""""""""
"""" Plugin Configuration """"
""""""""""""""""""""""""""""""
filetype off            " Turn filetype plugin off until Pathogen loads

" load packages
if &loadplugins
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

      " Neovim-only packages
      if has('nvim')
      endif

      " Enable coc.nvim if node.js is installed
      if executable('node')
          packadd! coc.nvim
      endif

      " colorschemes are automatically discovered
      " packadd! jellybeans.vim
      " packadd! seoul256.vim
      " packadd! tomorrow-theme
      " packadd! vim-colors-solarized
      " packadd! vim-lucius
  else
      " pathogen fallback
      " Unconventional path to plugin (inside submodule)
      runtime pack/bundle/opt/vim-pathogen/autoload/pathogen.vim

      " pathogen: Activate plugin
      call pathogen#infect('pack/bundle/opt/{}')
      call pathogen#helptags()
  endif
endif

syntax enable               " Enable syntax highlighting
filetype plugin indent on   " Enable filetype plugin and filetype-based indentation
silent! helptags ALL        " Generate documentation tages automatically

"ale settings
let g:ale_lint_on_text_changed = "never"        " Lint only when files are saved; linting when text changes is overkill/ annoying
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter = 0
let g:ale_open_list = 1                         " Show ALE messages in a loclist pane
let g:ale_echo_msg_error_str = 'E'              " Define how ALE errors and warnings are displayed in the statusline
let g:ale_echo_msg_warning_str = 'W'
let g:ale_statusline_format = ['%dE', '%dW', 'OK']
let g:ale_echo_msg_format = '[%linter%] [%severity%] %s'
let g:ale_disable_lsp = 1                       " Use ALE for linting, coc.nvim for LSP autocomplete

function! ALEc ()                                " ALE: C
    let g:ale_c_gcc_options = '-Wall -Wextra -pedantic -g -ggdb -std=c11'
    let g:ale_c_clang_options = '-Weverything -g -std=c11 -Wall'
endfunction

function! ALEcpp ()                              " ALE: C++
    let g:ale_cpp_gcc_options = '--Wall -Wextra -pedantic -g -ggdb -std=c++14'
    let g:ale_cpp_clang_options = '-Weverything -g -std=c++14'
endfunction

function! ALELinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'OK' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

autocmd BufRead,BufNewFile *.c call ALEc()
autocmd BufRead,BufNewFile *.cpp call ALEcpp()

" coc.nvim settings
" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
" CursorHold events and writing to swap files
set updatetime=500

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Slight change to also prevent trigger after a number
" when going from normal mode to insert mode
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s' || getline('.')[col - 1]  =~# '\d'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
"
" ctrlp settings
let g:ctrlp_map = '<leader><space>'
let g:ctrlp_show_hidden = 1

"rainbow parentheses settings
let g:rainbow_active = 1
" guifgs are RBG color wheel complements:
" red, green, blue-green, red-orange, blue, orange, violet, yellow, red-violet, yellow-green
" ctermfgs are 24-bit equivalent of the first 16 colors of 256-color chart
" '0' black and '8' grey don't show up well on terminals, so 14/16 used
let g:rainbow_conf = {
\   'guifgs'     : ['#FE2712', '#66B032', '#0392CE', '#FD5308', '#0247FE', '#FB9902', '#8601AF', '#FEFE33', '#A7194B', '#D0EA2B'],
\   'ctermfgs'   : ['1', '2', '3', '4', '5', '6', '7', '9', '10', '11' ,'12', '13', '14', '15'],
\   'separately' : {
\       '*'   : {
\               'guifgs'     : ['#FE2712', '#66B032', '#0392CE', '#FD5308', '#0247FE', '#FB9902', '#8601AF', '#FEFE33', '#A7194B', '#D0EA2B'],
\               'ctermfgs'   : ['1', '2', '3', '4', '5', '6', '7', '9', '10', '11' ,'12', '13', '14', '15'],
\       },
\       'lisp': {
\               'guifgs'     : ['#FE2712', '#66B032', '#0392CE', '#FD5308', '#0247FE', '#FB9902', '#8601AF', '#FEFE33', '#A7194B', '#D0EA2B'],
\               'ctermfgs'   : ['1', '2', '3', '4', '5', '6', '7', '9', '10', '11' ,'12', '13', '14', '15'],
\       },
\       'tex'  : {
\           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
\       },
\       'vim'  : {
\           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
\       },
\       'xml'  : {
\           'parentheses': ['start=/\v\<\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'))?)*\>/ end=#</\z1># fold'],
\       },
\       'xhtml': {
\           'parentheses': ['start=/\v\<\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'))?)*\>/ end=#</\z1># fold'],
\       },
\       'html' : {
\           'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
\       },
\       'php'  : {
\           'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold', 'start=/(/ end=/)/ containedin=@htmlPreproc contains=@phpClTop', 'start=/\[/ end=/\]/ containedin=@htmlPreproc contains=@phpClTop', 'start=/{/ end=/}/ containedin=@htmlPreproc contains=@phpClTop'],
\       },
\       'css'  : 0
\   }
\}

"slimv settings
let g:slimv_preferred = 'sbcl'                  " Prefer sbcl implementation
let g:slimv_repl_syntax = 1 " Enable syntax highlighting in Slimv REPL buffer
"let g:slimv_swank_cmd = '! xterm -e sbcl --load $HOME/dotfiles/emacs.d/plugins/slime/start-swank.lisp &'

"vim-indent-guides settings
let g:indent_guides_guide_size = 1              " Instead of having the indent guide be the size of a tabwidth, size it to just 1 character
let g:indent_guides_start_level = 2             " Start indent guides at 2nd-level of indents (we don't really need it in the first level)

"vim-latex settings
let g:tex_flavor='latex'    " Default .tex filetype to 'tex' instead of 'plaintex'
set shellslash              " Windows feature: forward slash when expanding filenames

"vim-schlepp settings
let g:Schlepp#dupTrimWS = 1                     " Trailing whitespace removal on block duplication
let g:Schlepp#reindent = 1                      " Reindent code as it is being moved
"vmap <unique> i <Plug>SchleppToggleReindent     " Toggle indentation of code
vmap <unique> <up>    <Plug>SchleppUp           " Drag visuals using arrow keys in visual mode
vmap <unique> <down>  <Plug>SchleppDown
vmap <unique> <left>  <Plug>SchleppLeft
vmap <unique> <right> <Plug>SchleppRight
vmap <unique> Dk <Plug>SchleppDupUp             " Duplication block bindings
vmap <unique> Dj <Plug>SchleppDupDown
vmap <unique> Dh <Plug>SchleppDupLeft
vmap <unique> Dl <Plug>SchleppDupRight

"solarized colorscheme settings
if !has("gui_running")                          " Use these settings if terminal vim; otherwise, gvimrc settings should take over
    let g:solarized_termcolors = 256            " Enable 256 color support and don't rely on terminal settings
    let g:solarized_termtrans = 0               " 1: Tell Solarized to use the default transparant terminal background; 0: Background handled by Vim
    let g:solarized_visibility = "normal"       " Set list characters can be set to one of three levels depending on your needs.
    let g:solarized_contrast = "normal"         " high/ low: Shifts some values up or down in order to expand or compress the tonal range displayed.
    "g:solarized_bold | g:solarized_underline | g:solarized_italic  " Set to 0 to turn of stylized typefaces
endif

""""""""""""""""""""""""""""""
"" End Plugin Configuration ""
""""""""""""""""""""""""""""""

"set ttyscroll=0         " Number of lines to scroll; 0 for terminals where scrolling is slow and redrawing is fast
"set ttyfast             " Fast terminal connection; more chars will be sent to screen for redrawing
set nostartofline       " Do not jump to first character with page navigation commands

set encoding=utf-8      " Default to UTF-8 encoding
set termencoding=utf-8
set fileencoding=utf-8
set fileformats=unix,dos,mac    " Default file formats

"set cindent             " No need to set cindent since it is automatic for C-based files
"set smartindent         " Smartindent: auto inserts one extra level of indentation for C-like files.
set autoindent          " Autoindent: auto indents a new line to the same indentation used by previous line
set nowrap              " Do not wrap lines
"set tw=500              " Word wrapping text width
set linespace=0         " Don't insert any extra pixel lines between rows
set linebreak

set expandtab           " Default tabs into spaces
set shiftwidth=4        " Auto-indent amount when using cindent, >>, << and stuff like that
set softtabstop=4       " When hitting a tab or backspace- spaces should a tab should be
set tabstop=8           " Vim's default tabstop is 8
set smarttab            " A <Tab> in front of a line inserts blanks according to settings; <BS> deletes a shiftwidth

set ignorecase          " Ignore case when searching
set smartcase           " If there are caps, go case-sensitive
set infercase           " Case inferred by default
set hlsearch            " Highlight search things
set incsearch           " Make search act like search in modern browsers

set showmatch           " Show matching brackets
set matchtime=1         " How many tenths of a second to show matching paren with showmatch set
let g:matchparen_insert_timeout=5   " Default is 60 -- will lag for extremely large files

set magic               " Set magic on, for regular expressions
set grepprg=/bin/grep\ -nH  " Vim grep

set list                " List mode: show special characters -- what is displayed is changed by listchars
                        " Inspired by https://old.reddit.com/r/vim/comments/4hoa6e/what_do_you_use_for_your_listchars/
set listchars=tab:⋮\ ,trail:·,nbsp:∘
set listchars+=extends:›,precedes:‹
"set listchars+=eol:¬    " or eol:$ or eol:↲

set laststatus=2        " Always show the statusline
set statusline+=[%n]\                                           "     n: Buffer number
set statusline+=%.64F%m%r%h%w\                                  "  .64F: Full path of file; max width of filepath, truncate from beginning
                                                                "     F: Full path of file
                                                                "     m: Modified flag in square brackets
                                                                "     r: Read-only flag in square brackets
                                                                "     h: Help flag in square brackets
                                                                "     w: Preview flag in square brackets
set statusline+=%{ALELinterStatus()}                            "     ALE plugin
set statusline+=%=%Y.%{&enc}.%{&ff}\                            "     =: Right justify from here
                                                                "     Y: Filetype as Vim-recognized
                                                                "  &enc: File encoding
                                                                "   &ff: File format
set statusline+=%<[utf8_0x%02.B\|ascii_%03.3b]                  "     <: Truncate from here
                                                                "  .02B: HEX value; max width of HEX value
                                                                " 03.3b: ASCII value; min and max width of ASCII value
set statusline+=[ln\ %02l:%02v/%L][%p%%]                        "   02l: Current line; min width
                                                                "   02v: Current column; min width
                                                                "     L: Total lines
                                                                "     p: Current position in file in percentage

set wildmenu                    " Turn on wild menu
set wildmode=list:longest,full  " Turn on wild mode huge list
set wildchar=<TAB>              " Path/file expansion in colon mode

" Ignore this list of file extensions
" */ for ctrlp globbing
set wildignore=*/.bak
set wildignore+=*/.sw?                                  " Vim swap files
set wildignore+=*/.DS_Store                             " OS X bullshit
set wildignore+=*/.spl                                  " Compiled spelling word lists
set wildignore+=*/.bmp,*/.gif,*/.jpg,*/.jpeg,*/.png     " Binary images
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*               " Version control
set wildignore+=*/.orig                                 " Merge resolution files
set wildignore+=*/.dll,*/.exe,*/.o,*/.obj,*/.manifest   " Compiled object files
set wildignore+=*/.pyc                                  " Python byte code
set wildignore+=migrations                              " Django migrations
set wildignore+=*/.luac                                 " Lua byte code
set wildignore+=*/.aux,*/.out,*/.toc                    " LaTeX intermediate files

set title               " Show title in console title bar
set guioptions=em       " e ensures tab bar is displayed in GUI; m ensures menu bar is displayed
set guitablabel=%t      " Display buffer name on tab label
set showcmd             " Display commands as they are typed
"set cmdheight=1         " Set the commandbar height -- default
set showtabline=2       " Display tab line even if single file opened
"set nohidden            " When I close a tab, remove from buffer
set tm=500              " Set tooltip menu

" Use xdiff patience algorithm
                        " See https://github.com/vim/vim/pull/2732
                        " and https://github.com/neovim/neovim/issues/1466
if has("patch-8.1.0360") || has('nvim')
    set diffopt+=vertical,internal,algorithm:patience
endif

set backup              " Keep backup file after overwriting a file
set writebackup         " Make a backup file before overwriting a file
set swapfile            " Allow creation of .swp swap files
if exists('+undofile')  " Have Vim save undo history to file and restore it on open
    set undofile
endif
"Default history and undolevels = 1000
"set history=350         " Sets how many lines of history Vim will remember
"set undolevels=500      " Number of undo levels

" Cache things to our home .cache dir
if isdirectory(expand('$HOME/.cache/vim'))
    if !has('nvim')     " Neovim defaults use $XDG_DATA_HOME/nvim
                        " i.e. ~/.local/share/nvim
        if &backupdir =~# '^\.,'
            set backupdir^=$HOME/.cache/vim/backup//
        endif

        if &directory =~# '^\.,'
            set directory^=$HOME/.cache/vim/swap//
        endif

        if exists('+undodir') && &undodir =~# '^\.\%(,\|$\)'
            set undodir^=$HOME/.cache/vim/undo//
        endif

        if exists('+viewdir')
            set viewdir=$HOME/.cache/vim/view//
        endif

    elseif has('nvim')  " Neovim backup default has local '.' precedence
                        " override to ~/.local/share/nvim/backup
        if &backupdir =~# '^\.,'
            set backupdir^=$HOME/.local/share/nvim/backup//
        endif

    endif
else
    call mkdir(expand("$HOME/.cache/vim"), "p")
    call mkdir(expand("$HOME/.cache/vim/backup"), "p")
    call mkdir(expand("$HOME/.cache/vim/swap"), "p")
    call mkdir(expand("$HOME/.cache/vim/undo"), "p")
    call mkdir(expand("$HOME/.cache/vim/view"), "p")

    if !has('nvim')
        set backupdir^=$HOME/.cache/vim/backup//
        set directory^=$HOME/.cache/vim/swap//
        set undodir^=$HOME/.cache/vim/undo//
        set viewdir=$HOME/.cache/vim/view//
    elseif has('nvim')
        set backupdir^=$HOME/.local/share/nvim/backup//
    endif
endif

if has('unnamedplus')
    set clipboard=unnamedplus   " Use the '+' register for all yank, delete, and change operations; the '+' register is the X Windows clipboard
elseif has('clipboard')
    set clipboard=unnamed       " Use the '*' register for all yank, delete, and change operations; the '*' register is the system clipboard
endif

set formatoptions=rw    " Automatically insert command leader on return and let gq format comments
set shiftround          " When at 3 spaces and hit > ... go to 4, not 5

if (&t_Co == 256 || &t_Co == 88) && (!has("gui_running")) && (($TERM =~ "xterm") || ($TERM =~ "screen") || ($TERM =~ "color"))
    " Colorscheme preferences:
    " solarized, jellybeans, Tomorrow-Night, seoul256, lucius, tango2, desert, torte, murphy
    if filereadable(expand("$HOME/.vim/pack/bundle/opt/vim-colors-solarized/colors/solarized.vim"))
        set background=dark
        colorscheme solarized
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/jellybeans.vim/colors/jellybeans.vim"))
        set background=dark
        colorscheme jellybeans
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/tomorrow-theme/colors/Tomorrow-Night.vim"))
        set background=light
        colorscheme Tomorrow-Night
    elseif filereadable(expand("$HOME/.vim/pack/bundle/opt/seoul256.vim/colors/seoul256.vim"))
        set background=dark
        colorscheme seoul256
    endif
endif

set cursorline          " Highlight the current line
set cursorcolumn        " Highlight the current column
""" Highlight column 81 for old-school 80 character terminal widths
""" The elseif hack is from http://stackoverflow.com/a/235970
if exists('+colorcolumn')
    set colorcolumn=81
else
    highlight ColorColumn ctermbg=235
    match ColorColumn /\%81v/
endif

if v:version > 704 || v:version == 704 && has("patch338")
    set breakindent showbreak=↪\   " Set breakindent with .. for wrapped lines
    "set breakindent showbreak=..\ 
    "set breakindent showbreak=\\ 
endif

""" relativenumber is a Vim 7.3 option
""" If the option doesn't exist, fall back to just absolute line numbers
if exists('+relativenumber') && (version >= 704) || (version == 703) && has("patch1115")
    set relativenumber  " Display how far away each line is from the current one by default
    set number          " When used with relativenumber, the absolute line of the current

elseif exists('+relativenumber')
    set relativenumber  "Display how far away each line is from the current one by default

    " Switch to absolute line numbers whenever Vim loses focus
    autocmd FocusLost * :set number
    autocmd FocusGained * :set relativenumber

    " Use absolute line numbers when in insert mode and relative numbers when in normal mode
    autocmd InsertEnter * :set norelativenumber | set number
    autocmd InsertLeave * :set relativenumber

else
    set number          " Show absolute line numbers
endif

" Use english for spellchecking, but don't spellcheck by default
if version >= 700
    set spl=en spell
    set nospell
else
    set spell spelllang=en_us
endif

set ruler               " Show cursor position all times
set numberwidth=5       " Aesthetic uses only... and for displaying large line numbers
set more                " Use more propmt -- pausing screen when too many items are displayed
set noautowrite         " Don't automagically write on :next
set hidden              " Change buffer - without saving

set lazyredraw          " Do not redraw while running macros; buffers screen updates

set backspace=indent,eol,start  " Set backspace config -- same as set bs=2
set whichwrap=b,s,h,l,<,>,[,]   " Move freely between file

set report=0            " Tell us when anything is changed via :...

"set scrolloff=0        " Set 'x' lines to the cursors position - when moving verticle
"set sidescrolloff=0    " Keep at least 'x' lines left/right

" Folding
set foldenable          " Turn on folding
set foldmarker={,}      " Fold C-style code (only user this as default if you use a high foldlevel
set foldmethod=marker   " Fold on the marker
set foldlevel=100       " Don't autofold anything (can still fold manually)
set foldopen=block,hor,mark,percent,quickfix,tag    " Define movement that opens folds

" No sound on errors
set noerrorbells
set novisualbell
set vb t_vb=            " When no beep or flash is wanted

set cpoptions+=a        " :read updates alternative file name
set cpoptions+=A        " :write updates alternative file name
set cpoptions+=B        " A backslash has no special meaning in mappings
set cpoptions+=c        " Searching continues at the end of the match at the cursor position
set cpoptions+=e        " Automatically add <CR> to the last line when using :@r
set cpoptions+=F        " :write command updates current file name
set cpoptions+=s        " Set buffer options when entering the buffer
set cpoptions+=m        " When a new match is created (showmatch), pause for .5s
set cpoptions+=q        " When joining lines, leave the cursor

" Map Ctrl-Backspace to delete previous word in insert mode like Ctrl-w
" Note how this mapping does not work in terminal vim because of term-keys
" However, this mapping will work just fine in gvim
inoremap    <C-BS>      <C-w>

" To move up and down logical lines instead of physical lines
" Instead of changing the Home row keys, use the arrow keys
nnoremap    <expr><Down>            (v:count == 0 ? 'gj' : 'j')
nnoremap    <expr><Up>              (v:count == 0 ? 'gk' : 'k')
vnoremap    <expr><Down>            (v:count == 0 ? 'gj' : 'j')
vnoremap    <expr><Up>              (v:count == 0 ? 'gk' : 'k')
noremap     <silent><expr><Down>    (v:count == 0 ? 'gj' : 'j')
noremap     <silent><expr><Up>      (v:count == 0 ? 'gk' : 'k')
inoremap    <Down>                  <C-o>gj
inoremap    <Up>                    <C-o>gk
inoremap    <silent><Up>            <C-o>gk
inoremap    <silent><Down>          <C-o>gj
" Home and end keys are broken -- dependent of TERM variable;
" Not sure how to fix reliably
noremap     <silent><Home>          g<Home>
noremap     <silent><End>           g<End>
inoremap    <silent><Home>          <C-o>g<Home>
inoremap    <silent><End>           <C-o>g<End>

" Smart way to move between windows
" Also noted how they don't seem to work... Need to investigate.
"noremap    <C-j>       <C-w>j
"noremap    <C-k>       <C-w>k
"noremap    <C-h>       <C-w>h
"noremap    <C-l>       <C-w>l

" Never "accidentally" enter ex mode
nnoremap    Q           <nop>

" From https://github.com/tommcdo/vimfiles/blob/master/config/consistency.vim
" Make Y behave like C and D
nnoremap    Y           y$

" Make cw behave like dw and yw
" NOTE: This causes some weird behaviour for end-of-line edge cases:
"   https://asciinema.org/a/9843
" To compensate for this edge case, use caw
"onoremap <silent> w :execute 'normal! '.v:count1.'w'<CR>

" From http://www.reddit.com/r/vim/comments/26nut8/why_does_cw_work_like_ce/chsz0pq
" What this does is it creates a new operator "z" that forcibly acts as a
" normal "w". This "w" is then mapped (only for cw, aka the "error") to cw to
" fix it. However it works perfectly fine with dw as well. It takes just one
" command history so undo works properly and dot repetition works as expected.
onoremap    <silent>z   :<C-U>normal! w<CR>
"map        cw          cz

" From https://stackoverflow.com/posts/3879737/revisions
" :hs command abbreviation/ alias for :split (horizontal split)
" Provides some consistency for :vs (shorthand for :vsplit, vertical split)
cnoreabbrev <expr> hs ((getcmdtype() is# ':' && getcmdline() is# 'hs')?('split'):('hs'))

if has("autocmd")
    " Don't use bottom restore cursor, Frew's JumpCursorOnEdit works fine
    " Discussion: http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/171558#171558
    " Restore cursor position
    " See https://github.com/bahamas10/dotfiles/blob/master/vimrc#L58
    "autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
    "autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"

    " help files
    au FileType                                     help set nonumber           " no line numbers when viewing help
    au FileType                                     help nnoremap <buffer><CR> <C-]>    " Enter selects subject
    au FileType                                     help nnoremap <buffer><BS> <C-T>    " Backspace to go back

    " TXT files
    "autocmd FileType                                text setlocal spell

    " makefiles
    " Makefiles are tab sensitive
    au FileType                                     make set noexpandtab

    " Rust
    au FileType                                     rust set noexpandtab

    " Go
    augroup go
        let g:metalinter_enabled = [
                    \ 'deadcode',
                    \ 'errcheck',
                    \ 'gosimple',
                    \ 'govet',
                    \ 'ineffassign',
                    \ 'staticcheck',
                    \ 'structcheck',
                    \ 'typecheck',
                    \ 'unused',
                    \ 'varcheck',
                    \ 'bodyclose',
                    \ 'depguard',
                    \ 'dogsled',
                    \ 'dupl',
                    \ 'gochecknoglobals',
                    \ 'gocognit',
                    \ 'goconst',
                    \ 'gocritic',
                    \ 'gocyclo',
                    \ 'gofmt',
                    \ 'golint',
                    \ 'gomnd',
                    \ 'goprintffuncname',
                    \ 'gosec',
                    \ 'interfacer',
                    \ 'maligned',
                    \ 'misspell',
                    \ 'nakedret',
                    \ 'prealloc',
                    \ 'rowserrcheck',
                    \ 'scopelint',
                    \ 'stylecheck',
                    \ 'unconvert',
                    \ 'unparam',
                    \ 'wsl'
                    \]
                    " Disabled because of unsupported formatting
                    "\ 'funlen',
                    "\ 'gochecknoinits',
                    "\ 'godox',
                    "\ 'goimports',
                    "\ 'lll',
                    "\ 'whitespace',

        let g:go_fmt_command = "goimports"      " Automatically format and rewrite import declarations
        "let g:go_auto_type_info = 1             " Automatically show identifier info whenever you move your cursor
        let g:go_doc_popup_window = 1           " Use popup-window for |K| and |:GoDoc| instead of |preview-window|
        let g:go_code_completion_enabled = 0    " Use coc.nvim for LSP autocomplete
        let g:go_def_mapping_enabled = 0        " Use coc.nvim for goto-definition

        " Quickfix list shortcuts
        nmap        <C-n>       :cnext<CR>
        nmap        <C-m>       :cprevious<CR>
        nnoremap    <leader>a   :cclose<CR>

        " Command shortcuts
        nmap <leader>r <Plug>(go-run)
        nmap <leader>c <Plug>(go-coverage-toggle)
        nmap <leader>i <Plug>(go-info)
        command! -bang A  call go#alternate#Switch(<bang>0, 'edit')
        command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
        command! -bang AS call go#alternate#Switch(<bang>0, 'split')
        command! -bang AT call go#alternate#Switch(<bang>0, 'tabe')

        set autowrite                       " Save when calling :GoBuild
        " run :GoBuild or :GoTestCompile based on the go file
        function! s:build_go_files()
            let l:file = expand('%')
            if l:file =~# '^\f\+_test\.go$'
                call go#test#Test(0, 1)
            elseif l:file =~# '^\f\+\.go$'
                call go#cmd#Build(0)
            endif
        endfunction
        nmap <leader>b :<C-u>call <SID>build_go_files()<CR>

    augroup END

    " Objective C / C++
    autocmd BufNewFile,BufReadPre,FileReadPre       *.m    setlocal filetype=objc
    autocmd BufNewFile,BufReadPre,FileReadPre       *.mm   setlocal filetype=objcpp
    autocmd FileType                                objc   setlocal sw=4 sts=4 et
    autocmd FileType                                objcpp setlocal sw=4 sts=4 et

    " Python
    augroup python
        autocmd BufNewFile,BufReadPre,FileReadPre   *.py   setlocal filetype=python
        autocmd FileType                            python setlocal sw=4 sts=4 et
    augroup END

    " Ruby/Chef
    augroup ruby
        autocmd BufNewFile,BufRead                  *.rb setlocal filetype=ruby
        autocmd FileType                            ruby setlocal sw=2 sts=2 et
    augroup END

    " JavaScript files
    augroup javascript
        autocmd BufNewFile,BufReadPre,FileReadPre   *.js        setlocal filetype=javascript
        autocmd FileType                            javascript  setlocal sw=4 sts=4 et
    augroup END

    " JSON files
    augroup json
        autocmd BufNewFile,BufReadPre,FileReadPre   *.json  setlocal filetype=javascript
        autocmd FileType                            json    setlocal sw=2 sts=2 et
    augroup END

    " Yaml
    augroup yaml
        autocmd BufNewFile,BufRead                  *.yaml,*.yml    setlocal filetype=yaml
        autocmd FileType                            yaml            setlocal sw=2 sts=2 et
    augroup END

    " PHP
    augroup php
        autocmd BufReadPre,FileReadPre              *.php set tabstop=4
        autocmd BufReadPre,FileReadPre              *.php set expandtab
    augroup END
    "
    " Drupal *.module and *.engine files
    augroup module
        autocmd BufRead                             *.module,*.engine set filetype=php
    augroup END

    " Java
    augroup java
        autocmd BufReadPre,FileReadPre              *.java set tabstop=4
        autocmd BufReadPre,FileReadPre              *.java set expandtab
    augroup END

    " ANT build.xml
    augroup xml
        autocmd BufReadPre,FileReadPre              build.xml set tabstop=4
    augroup END

    " (J)Flex
    augroup lex
        "autocmd BufRead,BufNewFile *.flex,*.jflex set filetype=lex
        autocmd BufRead,BufNewFile                  *.flex,*.jflex set filetype=jflex
    augroup END

    " (Nu)SMV
    augroup smv
        autocmd BufRead,BufNewFile                  *.smv set filetype=smv
    augroup END

    " Jekyll posts ignore yaml headers
    autocmd BufNewFile,BufRead                      */_posts/*.md syntax match Comment /\%^---\_.\{-}---$/
    autocmd BufNewFile,BufRead                      */_posts/*.md syntax region lqdHighlight start=/^{%\s*highlight\(\s\+\w\+\)\{0,1}\s*%}$/ end=/{%\s*endhighlight\s*%}/

    " EJS javascript templates
    autocmd BufNewFile,BufRead,FileReadPre          *.ejs setlocal filetype=html

    " When using mutt, text width=72
    au FileType                                     mail,tex set textwidth=72

    " File formats
    au BufNewFile,BufRead                           *.pls            set syntax=dosini
    au BufNewFile,BufRead                           modprobe.conf    set syntax=modconf

endif

" From Frew's configuration on StackOverflow
" {{{Frew's Auto Commands, Misc Commands, and Functions
" Discussion: http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/171558#171558

" Automatically cd into the directory that the file is in
"set autochdir           " Below BufEnter command errors in paths with \[ because of invalid range eval
"autocmd BufEnter * if expand('%:\p') !~ '://' | execute "chdir ".escape(expand("%:p:h"), ' ') | endif
set noautochdir         " justinmk's vim-dirvish does not support autochdir yet; see https://github.com/justinmk/vim-dirvish/issues/19
augroup auto_ch_dir
    autocmd!
    autocmd BufEnter * silent! lcd %:p:h
augroup END

" Remove any trailing whitespace that is in the file and indent entire file and return to position
function! CleanTrailingWhitespace ()
    autocmd BufRead,BufWrite * :exe ":normal! ma" | silent! %s/\s\+$//ge | :exe ":normal! 'azz"
endfunction

" Disable autoindent on write for contributing to open source projects with own style guidelines
function! AutoindentFile ()
    autocmd BufWrite * if ! &bin && &filetype != "" && &filetype !~ '\(asm\|conf\|haskell\|html\|js\|markdown\|make\|perl\|python\|sh\|text\)' | :exe ":normal! gg=G" | :exe ":normal! 'azz" | endif
endfunction

" Restore cursor position to where it was before
augroup JumpCursorOnEdit
    au!
    autocmd BufReadPost *
                \ if expand("<afile>:p:h") !=? $TEMP |
                \   if line("'\"") > 1 && line("'\"") <= line("$") |
                \     let JumpCursorOnEdit_foo = line("'\"") |
                \     let b:doopenfold = 1 |
                \     if (foldlevel(JumpCursorOnEdit_foo) > foldlevel(JumpCursorOnEdit_foo - 1)) |
                \        let JumpCursorOnEdit_foo = JumpCursorOnEdit_foo - 1 |
                \        let b:doopenfold = 2 |
                \     endif |
                \     exe JumpCursorOnEdit_foo |
                \   endif |
                \ endif
    " Need to postpone using "zv" until after reading the modelines.
    autocmd BufWinEnter *
                \ if exists("b:doopenfold") |
                \   exe "normal zv" |
                \   if(b:doopenfold > 1) |
                \       exe  "+".1 |
                \   endif |
                \   unlet b:doopenfold |
                \ endif
augroup END

