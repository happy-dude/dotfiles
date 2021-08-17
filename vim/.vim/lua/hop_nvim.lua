-- hop.nvim config

require('hop').setup {
  -- keys default: 'asdghklqwertyuiopzxcvbnmfj'
  -- use colemak equivalent
  keys = 'arstdhneioqwfpgjluyzxcvbkm',
}

vim.api.nvim_set_keymap('n', '<Leader><Leader>/', "<cmd>lua require'hop'.hint_patterns()<cr>", {})
vim.api.nvim_set_keymap('n', '<Leader><Leader>f', "<cmd>lua require'hop'.hint_char1()<cr>", {})
vim.api.nvim_set_keymap('n', '<Leader><Leader>l', "<cmd>lua require'hop'.hint_lines()<cr>", {})
vim.api.nvim_set_keymap('n', '<Leader><Leader>w', "<cmd>lua require'hop'.hint_words()<cr>", {})
