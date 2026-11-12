-- nvim-treesitter config

local api = vim.api

require('matchtag')

-- Home Manager links the parsers and queries; nothing installs or updates
-- them at run time.

-- nvim-treesitter ships features disabled; start highlighting when a parser is
-- available.
local treesitter_start_group = api.nvim_create_augroup('TreesitterStart', { clear = true })
api.nvim_create_autocmd('FileType', {
  group = treesitter_start_group,
  callback = function(args)
    local bufnr = args.buf
    local filetype = args.match

    -- Let every FileType consumer attach before the initial parser run.
    vim.schedule(function()
      if
        not api.nvim_buf_is_valid(bufnr)
        or not api.nvim_buf_is_loaded(bufnr)
        or vim.bo[bufnr].filetype ~= filetype
      then
        return
      end

      local lang = vim.treesitter.language.get_lang(filetype)
      local active = vim.treesitter.highlighter.active[bufnr]
      -- VimTeX's math text objects and conceal read its own syntax groups,
      -- which Tree-sitter highlighting would switch off.
      if not lang or lang == 'latex' then
        if active then
          vim.treesitter.stop(bufnr)
        end
        return
      end

      if active then
        if active.tree:lang() == lang then
          return
        end
        vim.treesitter.stop(bufnr)
      end

      -- Unsupported filetypes are expected to have no parser.
      pcall(vim.treesitter.start, bufnr, lang)
    end)
  end,
  desc = 'Start Tree-sitter highlighting when a parser is available',
})

-- nvim-treesitter-textobjects
require('nvim-treesitter-textobjects').setup({
  select = {
    lookahead = true,
  },
})

-- The plugin has no keymaps option on its main branch; declare the text
-- objects directly. Moving them back into setup() would silently do nothing.
local ts_select = require('nvim-treesitter-textobjects.select')
-- Capitalised so they neither shadow f{char} in Visual mode nor make c
-- wait for a second key; CoC's if/af/ic/ac stay the LSP-backed objects. In
-- Lisp buffers vim-sexp's buffer-local aF/iF (top-level form) take
-- precedence, which is the right object there.
for lhs, capture in pairs({
  aF = '@function.outer',
  iF = '@function.inner',
  aC = '@class.outer',
  iC = '@class.inner',
}) do
  vim.keymap.set({ 'x', 'o' }, lhs, function()
    ts_select.select_textobject(capture, 'textobjects')
  end, { desc = 'Select ' .. capture })
end

-- Rename through CoC, falling back to the native LSP client.
vim.keymap.set('n', 'gs', function()
  -- Use coc.nvim rename if available, fall back to native LSP
  if vim.fn.exists('*CocActionAsync') == 1 then
    vim.fn.CocActionAsync('rename')
  else
    vim.lsp.buf.rename()
  end
end, { desc = 'Smart rename (LSP)' })

-- rainbow-delimiters settings
-- The colours come from lib/rainbow-delimiters.nix, which vim/default.nix
-- sets as g:dotfiles_rainbow_palette. The group names are numbered by depth
-- rather than named after a colour, because a palette change would
-- otherwise leave names such as RainbowDelimiterRed holding another colour.
local rainbow_palette = vim.g.dotfiles_rainbow_palette or {}

local rainbow_highlights = {}
for depth in ipairs(rainbow_palette) do
  rainbow_highlights[depth] = 'RainbowDelimiterDepth' .. depth
end

-- Reapplied on every :colorscheme, which clears groups it does not define.
local function apply_rainbow_highlights()
  for depth, colour in ipairs(rainbow_palette) do
    api.nvim_set_hl(0, rainbow_highlights[depth], {
      fg = colour.gui,
      ctermfg = colour.cterm,
    })
  end
end

local rainbow_highlight_group =
  api.nvim_create_augroup('RainbowDelimiterHighlights', { clear = true })
api.nvim_create_autocmd('ColorScheme', {
  group = rainbow_highlight_group,
  callback = apply_rainbow_highlights,
  desc = 'Reapply custom rainbow delimiter highlights',
})
apply_rainbow_highlights()

vim.g.rainbow_delimiters = {
  strategy = {
    [''] = 'rainbow-delimiters.strategy.global',
    vim = 'rainbow-delimiters.strategy.local',
  },
  query = {
    [''] = 'rainbow-delimiters',
    lua = 'rainbow-blocks',
  },
  highlight = rainbow_highlights,
}
