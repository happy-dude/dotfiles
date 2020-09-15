" ctrlp settings

let g:ctrlp_map = '<leader><space>'
let g:ctrlp_show_hidden = 1

if executable('rg')
    " if the full path of working buffer matches regex '/\/.',
    " which is a hidden file, directory, or within a hidden directory
    " set grep program with '--hidden' flag
    if expand('%:p') =~# '\/\.'
        let g:ctrlp_user_command = 'rg %s --files --hidden --color=never --glob ""'
    else
        let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
    endif

    let g:ctrlp_use_caching = 0
endif
