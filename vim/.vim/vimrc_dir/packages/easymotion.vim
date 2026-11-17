" vim-easymotion settings (Vim only; Neovim uses hop.nvim from lua/hop_nvim.lua)

" Match hop.nvim's Colemak hint labels. The plugin reads this when it loads.
let g:EasyMotion_keys = 'arstdhneioqwfpgjluyzxcvbkm'

" The keys of hop.nvim's line and search hints. EasyMotion's search takes one
" or more characters ended by <CR>, where hop prompts for a pattern.
nmap <Leader><Leader>l <Plug>(easymotion-bd-jk)
nmap <Leader><Leader>/ <Plug>(easymotion-sn)
