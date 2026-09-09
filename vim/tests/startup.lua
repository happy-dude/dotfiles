-- Configuration that must be in effect once every plugin has loaded.
assert(vim.g.colors_name == 'gruvbox-material', 'the colorscheme was not applied')
assert(vim.o.termguicolors, 'termguicolors is off under a colour terminal')
