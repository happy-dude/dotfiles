" custom functions

" Remove any trailing whitespace that is in the file and indent entire file and return to position
function! StripTrailingWhitespace()
    if !&binary && &filetype != 'diff'
        " Preparation: save last search, and cursor position.
        let _s=@/
        let l = line(".")
        let c = col(".")
        " do the business:
        %s/\s\+$//e
        " clean up: restore previous search history, and cursor position
        let @/=_s
        call cursor(l, c)
    endif
endfunction

" Disable autoindent on write for contributing to open source projects with own style guidelines
function! AutoindentFile ()
    autocmd BufWrite * if ! &bin && &filetype != "" && &filetype !~ '\(asm\|conf\|haskell\|html\|js\|markdown\|make\|perl\|python\|sh\|text\)' | :exe ":normal! gg=G" | :exe ":normal! 'azz" | endif
endfunction

" Use Perl regex for search-and-replace
" Usage :S/pattern/replace/flags
" Supports ranges
" ref: https://vim.fandom.com/wiki/Perl_compatible_regular_expressions
if executable('perl')
    function s:PerlSubstitute(line1, line2, sstring)
        " Save cursor position
        let l = line(".")
        let c = col(".")

        let l:lines=getline(a:line1, a:line2)

        " Perl command with 'utf8' enabled
        " '#line 1' makes error messages prettier, displayed below:
        " Substitution replacement not terminated at PerlSubstitute line 1.
        let l:sysresult=system("perl -e 'use utf8;' -e '#line 1 \"PerlSubstitute\"' -pe ".
                    \shellescape("s".escape(a:sstring,"%!").";"), l:lines)
        if v:shell_error
            echo l:sysresult
            return
        endif

        let l:result=split(l:sysresult, "\n", 1)    " 1: don't drop blank lines

                                                    " delete lines but don't put in register
        execute a:line1.",".a:line2." normal \"_dd"
        call append(a:line1-1, l:result)            " add lines
        call cursor(a:line1, 1)                     " return cursor to starting position

        if a:line1 == a:line2
            echom "Substitution on line" a:line1
        else
            echom "Substitution on lines" a:line1 "to" a:line2
        endif

        " Restore cursor position
        call cursor(l, c)

    endfunction

    command -range -nargs=1 S call s:PerlSubstitute(<line1>, <line2>, <q-args>)
endif
