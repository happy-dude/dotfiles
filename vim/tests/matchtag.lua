local source = debug.getinfo(1, 'S').source:sub(2)
local repo = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(source)))

local treesitter_runtime = vim.env.MATCHTAG_TREESITTER_RUNTIME
  or (vim.env.HOME .. '/.local/share/nvim/site')
vim.opt.runtimepath:append(treesitter_runtime)
vim.opt.runtimepath:append(repo .. '/vim/.vim')

local matchtag = require('matchtag')

local function extmarks(bufnr)
  return vim.api.nvim_buf_get_extmarks(bufnr, matchtag.namespace, 0, -1, { details = true })
end

local function set_buffer(filetype, text)
  vim.cmd('enew!')
  local bufnr = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, { text })
  vim.bo[bufnr].filetype = filetype
  assert(vim.treesitter.get_parser(bufnr, filetype):parse()[1])
  return bufnr
end

local function check(filetype, text, cursor, expected_columns)
  local bufnr = set_buffer(filetype, text)
  vim.api.nvim_win_set_cursor(0, cursor)
  matchtag.update(bufnr)

  local marks = extmarks(bufnr)
  assert(
    #marks == #expected_columns,
    string.format('expected %d marks, got %d', #expected_columns, #marks)
  )
  for index, column in ipairs(expected_columns) do
    assert(
      marks[index][3] == column,
      string.format('mark %d: expected column %d, got %d', index, column, marks[index][3])
    )
  end
end

check('html', '<div><div>x</div></div>', { 1, 1 }, { 1, 19 })
check('html', '<div><div>x</div></div>', { 1, 15 }, { 6, 13 })
check('xml', '<root><item>x</item></root>', { 1, 8 }, { 7, 15 })
check('html', '<div><br /></div>', { 1, 7 }, {})

local bufnr = set_buffer('html', '<div><span>x</span></div>')
vim.api.nvim_exec_autocmds('FileType', { pattern = 'html' })
vim.api.nvim_win_set_cursor(0, { 1, 7 })
vim.api.nvim_exec_autocmds('CursorMoved', { buffer = bufnr })
assert(#extmarks(bufnr) == 2, 'CursorMoved did not highlight tag pair')
vim.api.nvim_exec_autocmds('BufLeave', { buffer = bufnr })
assert(#extmarks(bufnr) == 0, 'BufLeave did not clear tag highlights')

print('MatchTag Tree-sitter tests passed')
