" custom autocommands and filetype settings

if has("autocmd")
    " Don't use bottom restore cursor, Frew's JumpCursorOnEdit works fine
    " Discussion: http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/171558#171558
    " Restore cursor position
    " See https://github.com/bahamas10/dotfiles/blob/master/vimrc#L58
    "autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
    "autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"

    " help files
    au FileType                                     help set nonumber           " no line numbers when viewing help
    au FileType                                     help nnoremap <buffer><CR> <C-]>    " Enter selects subject
    au FileType                                     help nnoremap <buffer><BS> <C-T>    " Backspace to go back

    " TXT files
    "autocmd FileType                                text setlocal spell

    " makefiles
    " Makefiles are tab sensitive
    au FileType                                     make set noexpandtab

    " Rust
    au FileType                                     rust set noexpandtab

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

    " Objective C / C++
    autocmd BufNewFile,BufReadPre,FileReadPre       *.m    setlocal filetype=objc
    autocmd BufNewFile,BufReadPre,FileReadPre       *.mm   setlocal filetype=objcpp
    autocmd FileType                                objc   setlocal sw=4 sts=4 et
    autocmd FileType                                objcpp setlocal sw=4 sts=4 et

    " Python
    augroup python
        autocmd BufNewFile,BufReadPre,FileReadPre   *.py   setlocal filetype=python
        autocmd FileType                            python setlocal sw=4 sts=4 et
    augroup END

    " Ruby/Chef
    augroup ruby
        autocmd BufNewFile,BufRead                  *.rb setlocal filetype=ruby
        autocmd FileType                            ruby setlocal sw=2 sts=2 et
    augroup END

    " JavaScript files
    augroup javascript
        autocmd BufNewFile,BufReadPre,FileReadPre   *.js        setlocal filetype=javascript
        autocmd FileType                            javascript  setlocal sw=4 sts=4 et
    augroup END

    " JSON files
    augroup json
        autocmd BufNewFile,BufReadPre,FileReadPre   *.json  setlocal filetype=javascript
        autocmd FileType                            json    setlocal sw=2 sts=2 et
    augroup END

    " Yaml
    augroup yaml
        autocmd BufNewFile,BufRead                  *.yaml,*.yml    setlocal filetype=yaml
        autocmd FileType                            yaml            setlocal sw=2 sts=2 et
    augroup END

    " PHP
    augroup php
        autocmd BufReadPre,FileReadPre              *.php set tabstop=4
        autocmd BufReadPre,FileReadPre              *.php set expandtab
    augroup END
    "
    " Drupal *.module and *.engine files
    augroup module
        autocmd BufRead                             *.module,*.engine set filetype=php
    augroup END

    " Java
    augroup java
        autocmd BufReadPre,FileReadPre              *.java set tabstop=4
        autocmd BufReadPre,FileReadPre              *.java set expandtab
    augroup END

    " ANT build.xml
    augroup xml
        autocmd BufReadPre,FileReadPre              build.xml set tabstop=4
    augroup END

    " (J)Flex
    augroup lex
        "autocmd BufRead,BufNewFile *.flex,*.jflex set filetype=lex
        autocmd BufRead,BufNewFile                  *.flex,*.jflex set filetype=jflex
    augroup END

    " (Nu)SMV
    augroup smv
        autocmd BufRead,BufNewFile                  *.smv set filetype=smv
    augroup END

    " Jekyll posts ignore yaml headers
    autocmd BufNewFile,BufRead                      */_posts/*.md syntax match Comment /\%^---\_.\{-}---$/
    autocmd BufNewFile,BufRead                      */_posts/*.md syntax region lqdHighlight start=/^{%\s*highlight\(\s\+\w\+\)\{0,1}\s*%}$/ end=/{%\s*endhighlight\s*%}/

    " EJS javascript templates
    autocmd BufNewFile,BufRead,FileReadPre          *.ejs setlocal filetype=html

    " When using mutt, text width=72
    au FileType                                     mail,tex set textwidth=72

    " File formats
    au BufNewFile,BufRead                           *.pls            set syntax=dosini
    au BufNewFile,BufRead                           modprobe.conf    set syntax=modconf

endif

" From Frew's configuration on StackOverflow
" {{{Frew's Auto Commands, Misc Commands, and Functions
" Discussion: http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/171558#171558

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
