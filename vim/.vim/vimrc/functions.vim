" custom functions

" Remove any trailing whitespace that is in the file and indent entire file and return to position
function StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
        normal mz
        normal Hmy
        silent! %s/\s\+$//ge
        normal 'yz<CR>
        normal `z
    endif
endfunction

" Disable autoindent on write for contributing to open source projects with own style guidelines
function! AutoindentFile ()
    autocmd BufWrite * if ! &bin && &filetype != "" && &filetype !~ '\(asm\|conf\|haskell\|html\|js\|markdown\|make\|perl\|python\|sh\|text\)' | :exe ":normal! gg=G" | :exe ":normal! 'azz" | endif
endfunction
