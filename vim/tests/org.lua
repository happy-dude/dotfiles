local runtime = assert(vim.env.ORG_TREESITTER_RUNTIME, 'ORG_TREESITTER_RUNTIME is required')

vim.opt.runtimepath:prepend(runtime)
vim.cmd('enew')
vim.bo.filetype = 'org'
vim.api.nvim_buf_set_lines(0, 0, -1, false, {
  '* Parser check',
  '',
  'Tree-sitter must parse this Org buffer.',
})

local parser = vim.treesitter.get_parser(0, 'org')
assert(parser:parse()[1], 'Org parser returned no syntax tree')
