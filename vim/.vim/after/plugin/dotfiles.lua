-- Sourced once every package is on the runtimepath. The vimrc runs before
-- packages load, so the plugin modules required here do not exist yet then;
-- CoC scans its autoload sources at VimEnter, after this file.
vim.cmd('packadd! rustowl-nvim')
require('codecompanion_nvim')
require('init')
