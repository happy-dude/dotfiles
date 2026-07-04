function! coc#source#codecompanion#init() abort
  return v:lua.codecompanion_coc_init()
endfunction

function! coc#source#codecompanion#complete(opt, cb) abort
  return a:cb(v:lua.codecompanion_coc_complete(a:opt))
endfunction

function! coc#source#codecompanion#on_complete(opt) abort
  call v:lua.codecompanion_coc_execute(a:opt)
endfunction
