" vim-go settings

" Use the Nix-managed binary and let each project's golangci config select
" linters. The repository root enables all supported linters in .golangci.yml.
let g:go_metalinter_command = 'golangci-lint'

let g:go_fmt_command = "goimports"      " Automatically format and rewrite import declarations
"let g:go_auto_type_info = 1             " Automatically show identifier info whenever you move your cursor
let g:go_doc_popup_window = 1           " Use popup-window for |K| and |:GoDoc| instead of |preview-window|

set autowrite                       " Save when calling :GoBuild

function! s:BuildGoFiles() abort
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction

function! s:ConfigureGoBuffer() abort
  nmap <buffer> <localleader>r <Plug>(go-run)
  nmap <buffer> <localleader>c <Plug>(go-coverage-toggle)
  nmap <buffer> <localleader>i <Plug>(go-info)
  nnoremap <buffer> <localleader>b :<C-u>call <SID>BuildGoFiles()<CR>

  command! -buffer -bang A  call go#alternate#Switch(<bang>0, 'edit')
  command! -buffer -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
  command! -buffer -bang AS call go#alternate#Switch(<bang>0, 'split')
  command! -buffer -bang AT call go#alternate#Switch(<bang>0, 'tabe')
endfunction

augroup dotfiles_vim_go
  autocmd!
  autocmd FileType go,gomod,gohtmltmpl call <SID>ConfigureGoBuffer()
augroup END

if index(['go', 'gomod', 'gohtmltmpl'], &filetype) >= 0
  call s:ConfigureGoBuffer()
endif
