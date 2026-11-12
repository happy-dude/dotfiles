"rainbow parentheses settings

let g:rainbow_active = 1
" Colours come from lib/rainbow-delimiters.nix, which vim/default.nix sets as
" g:dotfiles_rainbow_palette before the vimrc sources this file. ctermfgs are
" indices into the terminal's 16-colour palette, for when 'termguicolors' is
" off. Without the variable, as when the vimrc is sourced directly, no colours
" are set here and the colorscheme's or the plugin's own apply.
"
" The plugin's defaults give some filetypes, such as lisp, colours of their
" own. A '*' entry, even an empty one, replaces those defaults for every
" filetype not named below, so the global colours reach all of them.
let g:rainbow_conf = {
      \   'separately' : {
      \       '*'    : {},
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
      \ }
if exists('g:dotfiles_rainbow_palette')
  let g:rainbow_conf.guifgs =
        \ map(copy(g:dotfiles_rainbow_palette), 'v:val.gui')
  let g:rainbow_conf.ctermfgs =
        \ map(copy(g:dotfiles_rainbow_palette), 'string(v:val.cterm)')
endif
