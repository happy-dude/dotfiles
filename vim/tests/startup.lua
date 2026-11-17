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

-- cache.vim must exclude the credential paths in lib/sensitive-paths.nix,
-- which the check names in DOTFILES_SENSITIVE_PATHS. The preamble hands it
-- that list before the vimrc runs, so compare the variable with the file,
-- then look for every entry in 'backupskip', which cache.vim fills while
-- the vimrc is sourced. tests/secret-state.vim covers what it does with
-- the list.
local sensitive_file =
  assert(os.getenv('DOTFILES_SENSITIVE_PATHS'), 'DOTFILES_SENSITIVE_PATHS is unset')
local sensitive = vim.json.decode(table.concat(vim.fn.readfile(sensitive_file), '\n'))
assert(
  vim.deep_equal(vim.g.dotfiles_sensitive_paths, sensitive),
  'the preamble does not carry the sensitive path list'
)
local skipped = {}
for _, pattern in ipairs(vim.opt.backupskip:get()) do
  skipped[pattern] = true
end
assert(#sensitive.anyDepthDirectories > 0, 'there are no any-depth directories')
for _, name in ipairs(sensitive.anyDepthDirectories) do
  assert(skipped['*/' .. name .. '/*'], name .. ' is not in backupskip')
end
for kind, root in pairs({ home = vim.fn.expand('~'), xdgConfig = vim.fn.expand('~/.config') }) do
  assert(#sensitive[kind].files > 0, kind .. ' has no files')
  assert(#sensitive[kind].directories > 0, kind .. ' has no directories')
  for _, name in ipairs(sensitive[kind].files) do
    assert(skipped[root .. '/' .. name], name .. ' is not in backupskip')
  end
  for _, name in ipairs(sensitive[kind].directories) do
    assert(skipped[root .. '/' .. name .. '/*'], name .. ' is not in backupskip')
  end
end

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

-- lua/qol.lua's dd sends only a plain dd on a blank line to the black-hole
-- register; a count (1dd included) or a named register keeps the stock dd.
local function after_dd(lines, keys)
  local scratch = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_current_buf(scratch)
  vim.api.nvim_buf_set_lines(scratch, 0, -1, false, lines)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  vim.fn.setreg('"', 'yanked')
  vim.fn.setreg('a', 'yanked')
  vim.api.nvim_feedkeys(keys, 'tx', false)
  local result = {
    lines = vim.api.nvim_buf_get_lines(scratch, 0, -1, false),
    unnamed = vim.fn.getreg('"'),
    a = vim.fn.getreg('a'),
  }
  vim.api.nvim_buf_delete(scratch, { force = true })
  return result
end
local function assert_dd(keys, lines, want_lines, register, want_register)
  local got = after_dd(lines, keys)
  assert(
    vim.deep_equal(got.lines, want_lines),
    string.format('%s left %s, not %s', keys, vim.inspect(got.lines), vim.inspect(want_lines))
  )
  assert(
    got[register] == want_register,
    string.format('%s left %s = %q, not %q', keys, register, got[register], want_register)
  )
end
-- The clipboard provider is absent here, so the unnamed register is ".
vim.o.clipboard = ''
assert_dd('dd', { '', 'keep' }, { 'keep' }, 'unnamed', 'yanked')
assert_dd('dd', { 'text', 'keep' }, { 'keep' }, 'unnamed', 'text\n')
assert_dd('1dd', { '', 'keep' }, { 'keep' }, 'unnamed', '\n')
assert_dd('2dd', { '', 'text', 'keep' }, { 'keep' }, 'unnamed', '\ntext\n')
assert_dd('"add', { '', 'keep' }, { 'keep' }, 'a', '\n')
