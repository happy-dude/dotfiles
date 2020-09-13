"vim-schlepp settings

let g:Schlepp#dupTrimWS = 1                     " Trailing whitespace removal on block duplication
let g:Schlepp#reindent = 1                      " Reindent code as it is being moved
"vmap <unique> i <Plug>SchleppToggleReindent     " Toggle indentation of code

"vmap <unique>   <up>        <Plug>SchleppUp         " Drag visuals using arrow keys in visual mode
"vmap <unique>   <down>      <Plug>SchleppDown
vmap <unique>   <left>      <Plug>SchleppLeft
vmap <unique>   <right>     <Plug>SchleppRight
vmap <unique>   Dk          <Plug>SchleppDupUp      " Duplication block bindings
vmap <unique>   Dj          <Plug>SchleppDupDown
vmap <unique>   Dh          <Plug>SchleppDupLeft
vmap <unique>   Dl          <Plug>SchleppDupRight
