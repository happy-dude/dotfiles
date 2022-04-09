-- orgmode.nvim config
-- ref: https://github.com/nvim-orgmode/orgmode#setup

-- Install tree-sitter-org
-- ref: https://github.com/milisims/tree-sitter-org#install
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'main',
    files = { 'src/parser.c', 'src/scanner.cc' },
  },
  filetype = 'org',
}

-- Load custom tree-sitter grammar for org filetype
require('orgmode').setup_ts_grammar()

require('orgmode').setup({
  org_agenda_files = {'~/org/*', '~/org/roam/**'},
  org_default_notes_file = '~/org/notes.org',

  -- https://github.com/nvim-orgmode/orgmode/issues/250
  org_todo_keywords = {'TODO', 'PROCESS', 'ORGANIZE', 'REVIEW', 'DO', '|', 'DONE'},               -- GTD
  org_todo_keyword_faces = {                      -- Xterm256 cterm codes
    TODO      = ':foreground 131 :weight bold',   -- IndianRed
    PROCESS   = ':foreground 63  :weight bold',   -- RoyalBlue1
    ORGANIZE  = ':foreground 172 :weight bold',   -- Orange3
    REVIEW    = ':foreground 171 :weight bold',   -- MediumOrchid1
    DO        = ':foreground 203 :weight bold',   -- IndianRed1
    DONE      = ':foreground 107 :weight bold'    -- DarkOliveGreen3
  }
})
