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
