" ack.vim settings

if executable('rg')
    " if the full path of working buffer matches regex '/\/.',
    " which is a hidden file, directory, or within a hidden directory
    " set grep program with '--hidden' flag
    if expand('%:p') =~# '\/\.'
        let g:ackprg = 'rg --color=never --vimgrep --no-heading --smart-case --hidden'
    else
        let g:ackprg = 'rg --color=never --vimgrep --no-heading --smart-case'
    endif
endif
