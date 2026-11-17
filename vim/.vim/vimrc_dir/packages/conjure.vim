" Conjure settings (Neovim only)

" Conjure reads this when its plugin loads. Limited to the Lisps, so other
" filetypes keep CoC's K and get no Conjure localleader mappings or REPL.
let g:conjure#filetypes = ['clojure', 'fennel', 'janet', 'hy', 'racket', 'scheme', 'lisp']
