" custom functions

" Remove trailing whitespace without changing the current view or search
function! StripTrailingWhitespace()
  if !&binary && &filetype !=# 'diff'
    let l:view = winsaveview()
    keepjumps keeppatterns %s/\s\+$//e
    call winrestview(l:view)
  endif
endfunction

" Use Perl regex for search-and-replace
" Usage :S/pattern/replace/flags
" Supports ranges
" ref:  https://vim.fandom.com/wiki/Perl_compatible_regular_expressions
"       https://blog.ostermiller.org/perl-wide-character-in-print/
if executable('perl')
  function s:PerlSubstitute(line1, line2, sstring) abort
    let l:lines = getline(a:line1, a:line2)

    " Perl command with 'utf8' enabled
    " -CSDA instructs Perl to treat standard input, file handles, and command line arguments as "UTF-8" by default
          " '#line 1' makes error messages prettier, displayed below:
          " Substitution replacement not terminated at PerlSubstitute line 1.
    let l:sysresult = systemlist("perl -CSDA -e 'use utf8;' -e '#line 1 \"PerlSubstitute\"' -pe ". shellescape("s".escape(a:sstring,"%!").";"), l:lines)
    if v:shell_error
      echo l:sysresult
      return
    endif

    " Perl can add or remove newlines. Insert its output after the addressed
    " range, then remove only that range, without changing any registers.
    if append(a:line2, l:sysresult)
      throw 'Unable to insert Perl substitution output'
    endif
    if !empty(l:sysresult)
      undojoin
    endif
    if deletebufline(bufnr('%'), a:line1, a:line2)
      throw 'Unable to remove Perl substitution input'
    endif
  endfunction

  command! -range -nargs=1 S call s:PerlSubstitute(<line1>, <line2>, <q-args>)
endif
