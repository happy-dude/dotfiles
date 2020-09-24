" vim-airline settings
" emulates personal custom statusline very closely; see statusline.vim

"let g:airline#extensions#tabline#enabled = 1                            " displays buffers in tabline
"let g:airline#extensions#tabline#formatter = 'unique_tail_improved'     " improve path format in tabline

let g:airline_theme = 'base16_gruvbox_dark_hard'                        " gruvbox theme
"let g:airline_theme = 'distinguished'

"let g:airline_highlighting_cache = 1                                    " performance: enable caching of the various syntax highlighting groups

" No need to distinguish between 'INSERT' and 'INSERT COMP' modes
" ref:  https://github.com/vim-airline/vim-airline/blob/master/autoload/airline/init.vim
"       https://github.com/vim-airline/vim-airline/blob/master/doc/airline.txt#L189
let g:airline_mode_map = {}
let g:airline_mode_map['ic'] = 'INSERT'
let g:airline_mode_map['ix'] = 'INSERT'

" function combining airline#parts#filetype() and airline#parts#ffenc()
" ref: https://github.com/vim-airline/vim-airline/blob/master/autoload/airline/parts.vim
function! Fileinfo()
  let filetype = (airline#util#winwidth() < 90 && strlen(&filetype) > 3)
        \ ? matchstr(&filetype, '...'). (&encoding is? 'utf-8' ? '…' : '>')
        \ : &filetype
  let expected = get(g:, 'airline#parts#ffenc#skip_expected_string', '')
  let bomb     = &l:bomb ? '[BOM]' : ''
  let ff       = strlen(&ff) ? '['.&ff.']' : ''
  let ffenc = (expected is# &fenc.bomb.ff) ? '' : &fenc.bomb.ff
  return filetype . '.' . ffenc
endfunction

" customize airline
function! AirlineCustomize()
  let spc = g:airline_symbols.space                                   " airline_symbols space char

  " define 'keyboard character' statusline symbol
  " keyboard                  = ⌨     = \u2328
  " insertion symbol          = ⎀     = \u2380    -- too tiny
  " bottom square bracket     = ⎵     = \u23b5    -- might work, but ambiguous
  " bottom over top bracket   = ⎶     = \u23b6
  if &encoding==?'utf-8' && !get(g:, "airline_symbols_ascii", 0)
    call extend(g:airline_symbols, {
          \ 'keychar' : "\u2328"
          \ }, 'keep')
  else
    call extend(g:airline_symbols, {
          \ 'keychar' : "u"
          \ }, 'keep')
  endif

  " define custom airline parts
  call airline#parts#define('bufnum', { 'raw': '[%n]' })                                          " buffer number
  call airline#parts#define('utf8val', { 'raw': g:airline_symbols.keychar . ' 0x%02.B' })         " utf8 value of char
  call airline#parts#define('lines', { 'raw': g:airline_symbols.linenr . '%02l:%02v/%L [%p%%]'})  " ln:col/TotLn [scroll%]
  call airline#parts#define_function('fileinfo', 'Fileinfo')                                      " filetype.encoding[fileformat]

  " replace airline sections with customized sections

  " buffer value with filepath
  " ref: https://github.com/vim-airline/vim-airline/blob/master/autoload/airline/init.vim
  if exists("+autochdir") && &autochdir == 1
    let g:airline_section_c = airline#section#create(['bufnum', spc, '%<', 'path', spc, 'readonly'])
  else
    let g:airline_section_c = airline#section#create(['bufnum', spc, '%<', 'path', spc, 'readonly', 'coc_status'])
  endif

  " utf8val added to default section_x parts, 'filetype' replaced
  let g:airline_section_x = airline#section#create_right(['bookmark', 'tagbar', 'vista', 'gutentags', 'omnisharp', 'grepper', 'utf8val'])

  " fileinfo replaces section_y
  let g:airline_section_y = airline#section#create_right(['fileinfo'])

  " add custom line:column format from statusline.vim
  if airline#util#winwidth() > 79
    let g:airline_section_z = airline#section#create(['windowswap', 'obsession', 'lines'])
  else
    let g:airline_section_z = airline#section#create(['lines'])
  endif
endfunction

" Initialize custom airline
autocmd User AirlineAfterInit call AirlineCustomize()
