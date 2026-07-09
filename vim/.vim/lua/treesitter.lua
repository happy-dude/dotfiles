-- nvim-treesitter config

local api = vim.api

-- Nix deployments link parsers and queries through Home Manager. Stow
-- deployments retain the nvim-treesitter :TSInstall/:TSUpdate workflow.

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

-- rainbow_delimiters settings
local rainbow_delimiters = require('rainbow-delimiters')

api.nvim_set_hl(0, 'RainbowDelimiterRed', { default = true, fg = '#FE2712', ctermfg = 'Red' })
api.nvim_set_hl(0, 'RainbowDelimiterWhite', { default = true, fg = '#d3d7cf', ctermfg = 'White' })
api.nvim_set_hl(0, 'RainbowDelimiterYellow', { default = true, fg = '#FCE94F', ctermfg = 'Yellow' })
api.nvim_set_hl(0, 'RainbowDelimiterGreen', { default = true, fg = '#66B032', ctermfg = 'Green' })
api.nvim_set_hl(0, 'RainbowDelimiterBlue', { default = true, fg = '#0392CE', ctermfg = 'Blue' })
api.nvim_set_hl(0, 'RainbowDelimiterViolet', { default = true, fg = '#75507b', ctermfg = 'DarkMagenta' })
api.nvim_set_hl(0, 'RainbowDelimiterDarkRed', { default = true, fg = '#FD5308', ctermfg = 'DarkRed' })
api.nvim_set_hl(0, 'RainbowDelimiterSilver', { default = true, fg = '#FFFFFF', ctermfg = 'White' })
api.nvim_set_hl(0, 'RainbowDelimiterOrange', { default = true, fg = '#FB9902', ctermfg = 'DarkYellow' })
api.nvim_set_hl(0, 'RainbowDelimiterDarkBlue', { default = true, fg = '#0247FE', ctermfg = 'DarkBlue' })
api.nvim_set_hl(0, 'RainbowDelimiterSeaGreen', { default = true, fg = '#D0EA2B', ctermfg = 'LightGreen' })
api.nvim_set_hl(0, 'RainbowDelimiterMagenta', { default = true, fg = '#A7194B', ctermfg = 'Magenta' })

--default values from plugin
--api.nvim_set_hl(0, 'RainbowDelimiterRed'   , {default = true, fg = '#cc241d', ctermfg= 'Red'    })
--api.nvim_set_hl(0, 'RainbowDelimiterOrange', {default = true, fg = '#d65d0e', ctermfg= 'White'  })
--api.nvim_set_hl(0, 'RainbowDelimiterYellow', {default = true, fg = '#d79921', ctermfg= 'Yellow' })
--api.nvim_set_hl(0, 'RainbowDelimiterGreen' , {default = true, fg = '#689d6a', ctermfg= 'Green'  })
--api.nvim_set_hl(0, 'RainbowDelimiterCyan'  , {default = true, fg = '#a89984', ctermfg= 'Cyan'   })
--api.nvim_set_hl(0, 'RainbowDelimiterBlue'  , {default = true, fg = '#458588', ctermfg= 'Blue'   })
--api.nvim_set_hl(0, 'RainbowDelimiterViolet', {default = true, fg = '#b16286', ctermfg= 'Magenta'})

-- Colors from nvim-ts-rainbow2
--  colors = { -- table of guifg values
--    '#FE2712',
--    '#66B032',
--    '#0392CE',
--    '#FD5308',
--    '#0247FE',
--    '#FB9902',
--    '#8601AF',
--    '#FEFE33',
--    '#A7194B',
--    '#D0EA2B',
--  },
--  termcolors = { -- table of ctermfg values
--    '1',
--    '2',
--    '3',
--    '4',
--    '5',
--    '6',
--    '7',
--    '9',
--    '10',
--    '11',
--    '12',
--    '13',
--    '14',
--    '15',
--  },

vim.g.rainbow_delimiters = {
  strategy = {
    [''] = rainbow_delimiters.strategy['global'],
    vim = rainbow_delimiters.strategy['local'],
  },
  query = {
    [''] = 'rainbow-delimiters',
    lua = 'rainbow-blocks',
  },
  highlight = {
    'RainbowDelimiterRed',
    'RainbowDelimiterWhite',
    'RainbowDelimiterYellow',
    'RainbowDelimiterGreen',
    'RainbowDelimiterBlue',
    'RainbowDelimiterViolet',
    'RainbowDelimiterDarkRed',
    'RainbowDelimiterSilver',
    'RainbowDelimiterOrange',
    'RainbowDelimiterDarkBlue',
    'RainbowDelimiterSeaGreen',
    'RainbowDelimiterMagenta',
  },
}
