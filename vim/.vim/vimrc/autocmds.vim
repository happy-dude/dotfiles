" custom autocommands and filetype settings

if has("autocmd")

  " TXT files
  "autocmd FileType text                              setlocal spell

  " help files
  au FileType help                                    set nonumber                    " no line numbers when viewing help
  au FileType help                                    nnoremap <buffer><CR> <C-]>     " Enter selects subject
  au FileType help                                    nnoremap <buffer><BS> <C-T>     " Backspace to go back

  " makefiles
  " Makefiles are tab sensitive
  au FileType make                                    set noexpandtab

  " Perl
  if executable('perltidy')

    command -range=% -nargs=* PerlTidy <line1>,<line2>!perltidy -st -q
    function PerlTidySave()
      let l = line(".")
      let c = col(".")
      :PerlTidy
      call cursor(l, c)
    endfun

    autocmd BufWrite *.pl                             :call PerlTidySave()
    au FileType perl                                  setlocal equalprg=perltidy\ -st\ -q
    au FileType perl                                  setlocal formatprg=perltidy\ -st\ -q

  endif

  " Don't use bottom restore cursor, Frew's JumpCursorOnEdit works fine
  " Discussion: http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/171558#171558
  " Restore cursor position
  " See https://github.com/bahamas10/dotfiles/blob/master/vimrc#L58
  "autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  "autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"

  " From Frew's configuration on StackOverflow
  " ref: http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/171558#171558
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

endif
