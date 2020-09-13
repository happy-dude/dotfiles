" statusline settings

set laststatus=2                                " Always show the statusline
set statusline+=[%n]\                               "     n: Buffer number
set statusline+=%.64F%m%r%h%w\                      "  .64F: Full path of file; max width of filepath, truncate from beginning
                                                    "     F: Full path of file
                                                    "     m: Modified flag in square brackets
                                                    "     r: Read-only flag in square brackets
                                                    "     h: Help flag in square brackets
                                                    "     w: Preview flag in square brackets
set statusline+=%{ALELinterStatus()}\               "     ALE plugin
set statusline+=%=%Y.%{&enc}.%{&ff}\                "     =: Right justify from here
                                                    "     Y: Filetype as Vim-recognized
                                                    "  &enc: File encoding
                                                    "   &ff: File format
set statusline+=%<[utf8_0x%02.B\|ascii_%03.3b]      "     <: Truncate from here
                                                    "  .02B: HEX value; max width of HEX value
                                                    " 03.3b: ASCII value; min and max width of ASCII value
set statusline+=[ln\ %02l:%02v/%L][%p%%]            "   02l: Current line; min width
                                                    "   02v: Current column; min width
                                                    "     L: Total lines
                                                    "     p: Current position in file in percentage
