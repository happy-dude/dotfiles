assert(vim.bo.filetype == 'org', 'Org filetype was not detected')
assert(_G.orgmode, 'orgmode.nvim was not configured before its filetype hook')
local parser = vim.treesitter.get_parser(0, 'org')
assert(parser:parse()[1], 'Org parser returned no syntax tree')
