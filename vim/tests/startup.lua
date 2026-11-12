-- Configuration that must be in effect once every plugin has loaded.
assert(vim.g.colors_name == 'gruvbox-material', 'the colorscheme was not applied')
assert(vim.o.termguicolors, 'termguicolors is off under a colour terminal')

-- rainbow-delimiters must cycle lib/rainbow-delimiters.nix, which the check
-- names in DOTFILES_RAINBOW_PALETTE, and the colours must survive a
-- colorscheme reload. The expected colours come from that file rather than
-- the variable the configuration reads, so a wrong preamble cannot agree
-- with itself.
local palette_file =
  assert(os.getenv('DOTFILES_RAINBOW_PALETTE'), 'DOTFILES_RAINBOW_PALETTE is unset')
local palette = vim.json.decode(table.concat(vim.fn.readfile(palette_file), '\n'))
assert(#palette > 0, 'the rainbow palette is empty')
local highlight = vim.g.rainbow_delimiters.highlight
assert(#highlight == #palette, 'rainbow-delimiters does not cycle the palette')
local function assert_rainbow_colours(when)
  for depth, colour in ipairs(palette) do
    local group = vim.api.nvim_get_hl(0, { name = highlight[depth] })
    assert(
      group.fg and string.format('#%06x', group.fg) == colour.gui:lower(),
      string.format('depth %d is not %s %s', depth, colour.gui, when)
    )
    assert(
      group.ctermfg == colour.cterm,
      string.format('depth %d is not terminal colour %d %s', depth, colour.cterm, when)
    )
  end
end
assert_rainbow_colours('after startup')
vim.cmd.colorscheme(vim.g.colors_name)
assert_rainbow_colours('after :colorscheme')

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
