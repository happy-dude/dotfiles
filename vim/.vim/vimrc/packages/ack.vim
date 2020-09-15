" ack.vim settings

if executable('rg')
    let g:ackprg = 'rg --color=never --vimgrep --no-heading --smart-case'
endif
