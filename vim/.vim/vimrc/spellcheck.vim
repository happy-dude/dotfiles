" spellcheck settings

" Use english for spellchecking, but don't spellcheck by default
if version >= 700
  set spl=en spell
  set nospell
else
  set spell spelllang=en_us
endif
