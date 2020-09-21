" vim-dirvish settings

" Automatically cd into the directory that the file is in
"set autochdir           " Below BufEnter command errors in paths with \[ because of invalid range eval
"autocmd BufEnter * if expand('%:\p') !~ '://' | execute "chdir ".escape(expand("%:p:h"), ' ') | endif
set noautochdir         " justinmk's vim-dirvish does not support autochdir yet; see https://github.com/justinmk/vim-dirvish/issues/19
augroup auto_ch_dir
    autocmd!
    autocmd BufEnter * silent! lcd %:p:h
augroup END
