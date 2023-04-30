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
  let g:ale_c_gcc_options =  '-std=c17 -g3 -ggdb3 -glldb -Wall -Wextra -pedantic -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=address,undefined -fsanitize-trap=alignment -fno-omit-frame-pointer'
  let g:ale_c_clang_options = '-std=c17 -g3 -ggdb3 -glldb -Weverything -pedantic -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=address,undefined -fsanitize-trap=alignment -fno-omit-frame-pointer'
endfunction

function! ALEcpp ()                             " ALE: C++
  let g:ale_cpp_gcc_options = '-std=c++20 -g3 -ggdb3 -glldb -Wall -Wextra -pedantic -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=address,undefined -fsanitize-trap=alignment -fno-omit-frame-pointer'
  let g:ale_cpp_clang_options =  '-std=c++20 -g3 -ggdb3 -glldb -Weverything -pedantic -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=address,undefined -fsanitize-trap=alignment -fno-omit-frame-pointer'
  let g:ale_cpp_clangtidy_options =   '-std=c++20 -g3 -ggdb3 -glldb -Weverything -pedantic -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=address,undefined -fsanitize-trap=alignment -fno-omit-frame-pointer -x c++'
  let g:ale_cpp_clangcheck_options = '-- -std=c++20 -g3 -ggdb3 -glldb -Weverything -pedantic -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=address,undefined -fsanitize-trap=alignment -fno-omit-frame-pointer -x c++'
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