" vim-go settings

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
      \ 'asciicheck',
      \ 'bodyclose',
      \ 'cyclop',
      \ 'depguard',
      \ 'dogsled',
      \ 'dupl',
      \ 'durationcheck',
      \ 'errorlint',
      \ 'exhaustive',
      \ 'exhaustivestruct',
      \ 'exportloopref',
      \ 'forbidigo',
      \ 'forcetypeassert',
      \ 'funlen',
      \ 'gci',
      \ 'gochecknoglobals',
      \ 'gochecknoinits',
      \ 'gocognit',
      \ 'goconst',
      \ 'gocritic',
      \ 'gocyclo',
      \ 'godot',
      \ 'godox',
      \ 'goerr113',
      \ 'gofmt',
      \ 'gofumpt',
      \ 'goheader',
      \ 'goimports',
      \ 'gomnd',
      \ 'gomoddirectives',
      \ 'gomodguard',
      \ 'goprintffuncname',
      \ 'gosec',
      \ 'ifshort',
      \ 'importas',
      \ 'lll',
      \ 'makezero',
      \ 'misspell',
      \ 'nakedret',
      \ 'nestif',
      \ 'nilerr',
      \ 'nlreturn',
      \ 'noctx',
      \ 'nolintlint',
      \ 'paralleltest',
      \ 'prealloc',
      \ 'predeclared',
      \ 'promlinter',
      \ 'revive',
      \ 'rowserrcheck',
      \ 'sqlclosecheck',
      \ 'stylecheck',
      \ 'tagliatelle',
      \ 'testpackage',
      \ 'thelper',
      \ 'tparallel',
      \ 'unconvert',
      \ 'unparam',
      \ 'wastedassign',
      \ 'whitespace',
      \ 'wrapcheck',
      \ 'wsl'
      \]

let g:go_fmt_command = "goimports"      " Automatically format and rewrite import declarations
"let g:go_auto_type_info = 1             " Automatically show identifier info whenever you move your cursor
let g:go_doc_popup_window = 1           " Use popup-window for |K| and |:GoDoc| instead of |preview-window|

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
