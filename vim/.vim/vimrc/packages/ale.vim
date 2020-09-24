"ale settings

let g:ale_open_list = 1                         " Show ALE messages in a loclist pane
let g:ale_lint_on_text_changed = "never"        " Lint only when files are saved; linting when text changes is overkill/ annoying
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter = 0

let g:ale_echo_msg_error_str = 'E'              " Define how ALE errors and warnings are displayed in the statusline
let g:ale_echo_msg_warning_str = 'W'
let g:ale_statusline_format = ['%dE', '%dW', 'OK']
let g:ale_echo_msg_format = '[%linter%] [%severity%] %s'

function! ALEc ()                               " ALE: C
  let g:ale_c_gcc_options = '-Wall -Wextra -pedantic -g -ggdb -std=c11'
  let g:ale_c_clang_options = '-Weverything -g -std=c11 -Wall'
endfunction

function! ALEcpp ()                             " ALE: C++
  let g:ale_cpp_gcc_options = '--Wall -Wextra -pedantic -g -ggdb -std=c++14'
  let g:ale_cpp_clang_options = '-Weverything -g -std=c++14'
endfunction

function! ALELinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))

  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors

  return l:counts.total == 0 ? 'OK' : printf(
        \ '%dW %dE',
        \ all_non_errors,
        \ all_errors
        \)
endfunction

autocmd BufRead,BufNewFile *.c call ALEc()
autocmd BufRead,BufNewFile *.cpp call ALEcpp()
