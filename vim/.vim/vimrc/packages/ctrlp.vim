" ctrlp settings

let g:ctrlp_map = '<leader><space>'
let g:ctrlp_show_hidden = 1

if executable('rg')
  set grepprg=rg\ --color=never\ --vimgrep\ --no-heading\ --smart-case
  set grepformat=%f:%l:%c:%m
  let g:ctrlp_user_command = 'rg %s --files --hidden --color=never --glob ""'
  let g:ctrlp_use_caching = 0
endif
