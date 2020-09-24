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
" ref:  https://vim.fandom.com/wiki/Perl_compatible_regular_expressions
"       https://github.com/vim/vim/blob/master/src/ex_cmds.c#L3547-L3556
"       https://github.com/neovim/neovim/blob/master/src/nvim/ex_cmds.c#L5781-L5786
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

    " delete and append lines from command
    " see :h append() for more
    if a:line1 == 1 && line('$') == 1
      " if buffer only has 1 line
      " delete line to leave empty blank line
      " append from beginning after blank line
      " delete the blank line at beginning of buffer

      execute a:line1.",".a:line2."d"
      call append(1, l:result)
      execute "1,1d"

    elseif a:line1 == a:line2
      " if we are substituting a single line
      " delete the single line
      " append after line that was left over

      execute a:line1.",".a:line2."d"
      call append(a:line1-1, l:result)

    elseif a:line1 == 1 && a:line2 == line('$')
      " if we are substituting the whole file
      " delete all lines to leave empty blank line
      " append lines before the blank line at beginning of buffer

      execute a:line1.",".a:line2."d"
      call append(0, l:result)

      " if we are substituting the newlines at the end of each line
      if (a:sstring == '/\n//')
        " delete the newline following line1 arg
        execute a:line1+1."d"
      else
        " otherwise
        " delete the blank line that was pushed to the end of buffer
        execute a:line2+1."d"
      endif

    elseif a:line1-1 >= 1
      " if we are substituting within the body of the buffer
      " we can delete lines and append without extra newlines showing up

      execute a:line1.",".a:line2."d"
      call append(a:line1-1, l:result)

    else
      " else, we are substituting a range that includes beginning of buffer
      " delete lines, moving leftover lines at beginning of buffer
      " append substitutions before the first line

      execute a:line1.",".a:line2."d"
      call append(0, l:result)
    endif

    call cursor(l, c)                           " restore cursor position

    if a:line1 == a:line2
      echom "Substitution on line" a:line1
    else
      echom "Substitution on lines" a:line1 "to" a:line2
    endif

  endfunction

  command -range -nargs=1 S call s:PerlSubstitute(<line1>, <line2>, <q-args>)
endif
