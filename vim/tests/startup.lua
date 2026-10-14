-- Configuration that must be in effect once every plugin has loaded.
assert(vim.g.colors_name == 'gruvbox-material', 'the colorscheme was not applied')
assert(vim.o.termguicolors, 'termguicolors is off under a colour terminal')

-- Two rapid FileType changes before scheduled callbacks run must leave the
-- buffer highlighted for its final filetype rather than the earlier one.
local buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(buf, 0, -1, false, { 'def f():', '    return 1' })
vim.bo[buf].filetype = 'lua'
vim.bo[buf].filetype = 'python'
vim.wait(200, function()
  return vim.treesitter.highlighter.active[buf] ~= nil
end)
assert(
  vim.treesitter.highlighter.active[buf]
    and vim.treesitter.highlighter.active[buf].tree:lang() == 'python',
  'rapid FileType change kept the stale Tree-sitter parser'
)
vim.api.nvim_buf_delete(buf, { force = true })

-- Deleting a buffer before its scheduled FileType callback runs must not
-- raise an invalid-buffer error in the event loop.
local wiped = vim.api.nvim_create_buf(false, true)
vim.bo[wiped].filetype = 'lua'
vim.api.nvim_buf_delete(wiped, { force = true })
vim.wait(50, function()
  return false
end)
