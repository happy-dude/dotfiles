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

    " Python
    augroup python
        autocmd BufNewFile,BufReadPre,FileReadPre *.py  setlocal filetype=python
        autocmd FileType python                         setlocal sw=4 sts=4 tw=79 et
    augroup END

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
