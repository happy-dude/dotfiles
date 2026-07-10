-- nvim-treesitter config

local api = vim.api

-- Nix deployments link parsers and queries through Home Manager. Stow
-- deployments retain the nvim-treesitter :TSInstall/:TSUpdate workflow.

-- nvim-treesitter ships features disabled; start highlighting when a parser is
-- available from either deployment.
local treesitter_start_group = api.nvim_create_augroup('TreesitterStart', { clear = true })
api.nvim_create_autocmd('FileType', {
  group = treesitter_start_group,
  callback = function(args)
    local bufnr = args.buf
    local filetype = args.match

    -- Let every FileType consumer attach before the initial parser run.
    vim.schedule(function()
      if not api.nvim_buf_is_valid(bufnr) then
        return
      end

      local lang = vim.treesitter.language.get_lang(filetype)
      if not lang then
        return
      end

      if vim.treesitter.highlighter.active[bufnr] then
        return
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
    enable = true,
    lookahead = true,
    keymaps = {
      ['fo'] = '@function.outer',
      ['fi'] = '@function.inner',
      ['co'] = '@class.outer',
      ['ci'] = '@class.inner',
    },
  },
})

-- LSP rename replacement for refactor.smart_rename
vim.keymap.set('n', 'gs', function()
  -- Use coc.nvim rename if available, fall back to native LSP
  if vim.fn.exists(':CocAction') == 2 then
    vim.fn.CocActionAsync('rename')
  else
    vim.lsp.buf.rename()
  end
end, { desc = 'Smart rename (LSP)' })

-- rainbow-delimiters settings
local rainbow_palette = {
  { name = 'RainbowDelimiterRed', fg = '#FE2712', ctermfg = 'Red' },
  { name = 'RainbowDelimiterWhite', fg = '#d3d7cf', ctermfg = 'White' },
  { name = 'RainbowDelimiterYellow', fg = '#FCE94F', ctermfg = 'Yellow' },
  { name = 'RainbowDelimiterGreen', fg = '#66B032', ctermfg = 'Green' },
  { name = 'RainbowDelimiterBlue', fg = '#0392CE', ctermfg = 'Blue' },
  { name = 'RainbowDelimiterViolet', fg = '#75507b', ctermfg = 'DarkMagenta' },
  { name = 'RainbowDelimiterDarkRed', fg = '#FD5308', ctermfg = 'DarkRed' },
  { name = 'RainbowDelimiterSilver', fg = '#FFFFFF', ctermfg = 'White' },
  { name = 'RainbowDelimiterOrange', fg = '#FB9902', ctermfg = 'DarkYellow' },
  { name = 'RainbowDelimiterDarkBlue', fg = '#0247FE', ctermfg = 'DarkBlue' },
  { name = 'RainbowDelimiterSeaGreen', fg = '#D0EA2B', ctermfg = 'LightGreen' },
  { name = 'RainbowDelimiterMagenta', fg = '#A7194B', ctermfg = 'Magenta' },
}

local rainbow_highlights = {}
for index, highlight in ipairs(rainbow_palette) do
  rainbow_highlights[index] = highlight.name
end

local function apply_rainbow_highlights()
  for _, highlight in ipairs(rainbow_palette) do
    api.nvim_set_hl(0, highlight.name, {
      fg = highlight.fg,
      ctermfg = highlight.ctermfg,
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
