local api = vim.api

local namespace = api.nvim_create_namespace('MatchTag')

-- Replace gregsexton/MatchTag with structural HTML and XML matching.
-- Tree-sitter identifies the containing element, including nested same-name
-- tags, and extmarks highlight both tag names with MatchParen.

local tag_shapes = {
  html = {
    opening = 'start_tag',
    closing = 'end_tag',
    name = 'tag_name',
  },
  xml = {
    opening = 'STag',
    closing = 'ETag',
    name = 'Name',
  },
}

local function clear(bufnr)
  api.nvim_buf_clear_namespace(bufnr, namespace, 0, -1)
end

local function find_tag(node, shape)
  while node do
    local node_type = node:type()
    if node_type == shape.opening or node_type == shape.closing then
      return node
    end
    node = node:parent()
  end
end

local function find_child(node, child_type)
  for child in node:iter_children() do
    if child:named() and child:type() == child_type then
      return child
    end
  end
end

local function highlight_node(bufnr, node)
  local start_row, start_col, end_row, end_col = node:range()
  api.nvim_buf_set_extmark(bufnr, namespace, start_row, start_col, {
    end_row = end_row,
    end_col = end_col,
    hl_group = 'MatchParen',
    priority = 200,
  })
end

local function update(bufnr)
  clear(bufnr)

  if vim.fn.pumvisible() == 1 then
    return
  end

  local shape = tag_shapes[vim.bo[bufnr].filetype]
  if not shape then
    return
  end

  local row, col = unpack(api.nvim_win_get_cursor(0))
  local node = vim.treesitter.get_node({
    bufnr = bufnr,
    pos = { row - 1, col },
  })
  local tag = find_tag(node, shape)
  if not tag then
    return
  end

  local element = tag:parent()
  if not element or element:type() ~= 'element' then
    return
  end

  local opening_tag = find_child(element, shape.opening)
  local closing_tag = find_child(element, shape.closing)
  if not opening_tag or not closing_tag then
    return
  end

  local opening_name = find_child(opening_tag, shape.name)
  local closing_name = find_child(closing_tag, shape.name)
  if not opening_name or not closing_name then
    return
  end

  highlight_node(bufnr, opening_name)
  highlight_node(bufnr, closing_name)
end

local group = api.nvim_create_augroup('MatchTag', { clear = true })
api.nvim_create_autocmd('FileType', {
  group = group,
  pattern = { 'html', 'xml' },
  callback = function(args)
    api.nvim_clear_autocmds({ group = group, buffer = args.buf })
    api.nvim_create_autocmd({
      'CursorMoved',
      'CursorMovedI',
      'TextChanged',
      'TextChangedI',
    }, {
      group = group,
      buffer = args.buf,
      callback = function(event)
        pcall(update, event.buf)
      end,
      desc = 'Highlight matching tags with Tree-sitter',
    })
    api.nvim_create_autocmd({ 'BufLeave', 'BufWinLeave' }, {
      group = group,
      buffer = args.buf,
      callback = function(event)
        clear(event.buf)
      end,
      desc = 'Clear matching tag highlights',
    })
  end,
})

return {
  clear = clear,
  namespace = namespace,
  update = update,
}
