-- nvim-treesitter config

local api = vim.api
local rainbow = require('ts-rainbow')

require('nvim-treesitter.install').compilers = { 'clang' }

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'main',
    files = { 'src/parser.c', 'src/scanner.c' },
  },
  filetype = 'org',
}

require('nvim-treesitter.configs').setup {
  ensure_installed = 'all', -- one of "all", "language", or a list of languages

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = true,

  -- List of parsers to ignore installing
  ignore_install = { "norg" },

  -- nvim-treesitter native modules
  highlight = {
    enable = true, -- false will disable the whole extension
    -- disable = { },           -- list of languages where highlighting will be disabled
    -- custom_captures = { }    -- custom highlight groups for captures
    additional_vim_regex_highlighting = { 'org' }, -- Required since TS highlighter doesn't support all orgmode.nvim syntax features (conceal)
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gi',
      node_incremental = 'n',
      node_decremental = 'N',
      scope_incremental = 'b',
    },
  },

  -- external modules
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        -- capture groups defined in textobjects.scm
        ['fo'] = '@function.outer',
        ['fi'] = '@function.inner',
        ['co'] = '@class.outer',
        ['ci'] = '@class.inner',
      },
    },
    lsp_interop = { enable = false },
    move = { enable = false },
    swap = { enable = false },
  },
  refactor = {
    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = 'gs',
      },
    },
    highlight_current_scope = { enable = false },
    highlight_definitions = { enable = false }, -- provided by coc.nvim
    navigation = { enable = false },
  },
  rainbow = {
    enable = true,
    query = {
      'rainbow-parens',
      html = 'rainbow-tags'
    },
    strategy = {
      rainbow.strategy['global'],
    },
    colors = { -- table of guifg values
      '#FE2712',
      '#66B032',
      '#0392CE',
      '#FD5308',
      '#0247FE',
      '#FB9902',
      '#8601AF',
      '#FEFE33',
      '#A7194B',
      '#D0EA2B',
    },
    termcolors = { -- table of ctermfg values
      '1',
      '2',
      '3',
      '4',
      '5',
      '6',
      '7',
      '9',
      '10',
      '11',
      '12',
      '13',
      '14',
      '15',
    },
  },
}

-- turn on treesitter folding for supported languages
-- ref: https://github.com/nvim-treesitter/nvim-treesitter/issues/475#issuecomment-748532035
local define_modules = require('nvim-treesitter').define_modules
local query = require 'nvim-treesitter.query'

local foldmethod_backups = {}
local foldexpr_backups = {}

define_modules {
  folding = {
    enable = true,
    attach = function(bufnr)
      -- Fold settings are actually window based...
      foldmethod_backups[bufnr] = vim.wo.foldmethod
      foldexpr_backups[bufnr] = vim.wo.foldexpr
      vim.wo.foldmethod = 'expr'
      vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
    end,
    detach = function(bufnr)
      vim.wo.foldmethod = foldmethod_backups[bufnr]
      vim.wo.foldexpr = foldexpr_backups[bufnr]
      foldmethod_backups[bufnr] = nil
      foldexpr_backups[bufnr] = nil
    end,
    is_supported = query.has_folds,
  },
}
