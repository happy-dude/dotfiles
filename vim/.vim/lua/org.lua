-- orgmode.nvim config
-- ref: https://github.com/nvim-orgmode/orgmode#setup

-- Install tree-sitter-org
-- ref: https://github.com/milisims/tree-sitter-org#install
--local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
--parser_config.org = {
--  install_info = {
--    url = 'https://github.com/milisims/tree-sitter-org',
--    revision = 'main',
--    files = { 'src/parser.c', 'src/scanner.cc' },
--  },
--  filetype = 'org',
--}

-- Load custom tree-sitter grammar for org filetype
-- require('orgmode').setup_ts_grammar()

require('orgmode').setup({
  org_agenda_files = { '~/org/*', '~/org/roam/**' },
  org_default_notes_file = '~/org/notes.org',

  -- https://github.com/nvim-orgmode/orgmode/issues/250
  org_todo_keywords = { 'TODO', 'PROCESS', 'ORGANIZE', 'REVIEW', 'DO', '|', 'DONE' }, -- GTD
  -- orgmode emits these as gui attributes under 'termguicolors', where a
  -- bare cterm index is dropped; name the xterm-256 colours as hex.
  org_todo_keyword_faces = {
    TODO = ':foreground #af5f5f :weight bold', -- IndianRed (131)
    PROCESS = ':foreground #5f5fff :weight bold', -- RoyalBlue1 (63)
    ORGANIZE = ':foreground #d78700 :weight bold', -- Orange3 (172)
    REVIEW = ':foreground #d75fff :weight bold', -- MediumOrchid1 (171)
    DO = ':foreground #ff5f5f :weight bold', -- IndianRed1 (203)
    DONE = ':foreground #87af5f :weight bold', -- DarkOliveGreen3 (107)
  },
})
