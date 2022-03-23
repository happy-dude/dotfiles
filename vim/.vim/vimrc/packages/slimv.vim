"slimv settings

let g:slimv_preferred = 'sbcl'                  " Prefer sbcl implementation
let g:slimv_repl_syntax = 1                     " Enable syntax highlighting in Slimv REPL buffer
"let g:slimv_swank_cmd = '! xterm -e $(which sbcl) --load "$HOME/dotfiles/emacs/.config/emacs/plugins/slime/start-swank.lisp" &'

" https://roswell.github.io/Initial-Recommended-Setup.html
"let g:slimv_swank_cmd = "! xterm -e ros -e '(ql:quickload :swank) (swank:create-server)' wait &"
"let g:slimv_lisp = 'ros run'
"let g:slimv_impl = 'sbcl'

" Use Olical/conjure for REPL environment; disable Slimv mappings
let g:slimv_keybindings = 0
