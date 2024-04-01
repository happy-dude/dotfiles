-- Pull in the wezterm API
local wezterm = require('wezterm')

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

config.enable_wayland = false

config.set_environment_variables = {
  TERMINFO_DIRS = '/home/schan/.terminfo',
}
config.term = 'wezterm'
config.automatically_reload_config = true

config.default_prog = { '/usr/bin/zsh', '--login', '-c', 'tmux attach -t "mux" || tmux new -s "mux"' }
wezterm.action.SpawnCommandInNewWindow({
  args = { 'zsh' },
})

config.window_padding = {
  left = '0.25cell',
  right = '0.25cell',
  top = '0.25cell',
  bottom = '0.25cell',
}

config.initial_cols = 120
config.initial_rows = 36

config.window_decorations = 'TITLE | RESIZE'
config.text_background_opacity = 1.0
config.scrollback_lines = 50000

config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true

config.color_scheme = 'Gruvbox Material (Gogh)'
config.font = wezterm.font('Fira Code')
config.font_size = 16.0
config.custom_block_glyphs = true
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }   -- disable ligatures

wezterm.on('open-uri', function(window, pane, uri)
  wezterm.open_with(uri, 'firefox-trunk')
end)

config.leader = {
  key = 'Escape',
  mods = 'CTRL',
  timeout_milliseconds = 1000,
}
config.keys = {
  {
    key = 'n',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.SpawnCommandInNewWindow({ args = { '/usr/bin/zsh', '--login' } }),
  },
  {
    key = 't',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.SpawnCommandInNewTab({ args = { '/usr/bin/zsh', '--login' } }),
  },
  -- Leader keys
  {
    key = '|',
    mods = 'LEADER|SHIFT',
    action = wezterm.action.SplitHorizontal({
      domain = 'CurrentPaneDomain',
      args = { '/usr/bin/zsh', '--login' },
    }),
  },
  {
    key = '-',
    mods = 'LEADER',
    action = wezterm.action.SplitVertical({
      domain = 'CurrentPaneDomain',
      args = { '/usr/bin/zsh', '--login' },
    }),
  },

  -- tmux
  {
    key = 'q',
    mods = 'CTRL',
    action = wezterm.action({ SendString = '\x11' }),
  },

  -- macOS: alt + left/right arrow to jump words
  {
    key = 'Space',
    mods = 'ALT',
    action = wezterm.action({ SendString = ' ' }),
  },
  {
    key = 'LeftArrow',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bb' }),
  },
  {
    key = 'RightArrow',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bf' }),
  },
  {
    key = 'LeftArrow',
    mods = 'CMD',
    action = wezterm.action({ SendString = '\x1bOH' }),
  },
  {
    key = 'RightArrow',
    mods = 'CMD',
    action = wezterm.action({ SendString = '\x1bOF' }),
  },
  {
    key = 'Backspace',
    mods = 'CMD',
    action = wezterm.action({ SendString = '\x15' }),
  },
  -- Map ALT/Meta key correctly
  {
    key = 'A',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1ba' }),
  },
  {
    key = 'B',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bb' }),
  },
  {
    key = 'C',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bc' }),
  },
  {
    key = 'D',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bd' }),
  },
  {
    key = 'E',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1be' }),
  },
  {
    key = 'F',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bf' }),
  },
  {
    key = 'G',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bg' }),
  },
  {
    key = 'H',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bh' }),
  },
  {
    key = 'I',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bi' }),
  },
  {
    key = 'J',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bj' }),
  },
  {
    key = 'K',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bk' }),
  },
  {
    key = 'L',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bl' }),
  },
  {
    key = 'M',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bm' }),
  },
  {
    key = 'N',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bn' }),
  },
  {
    key = 'O',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bo' }),
  },
  {
    key = 'P',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bp' }),
  },
  {
    key = 'Q',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bq' }),
  },
  {
    key = 'R',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1br' }),
  },
  {
    key = 'S',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bs' }),
  },
  {
    key = 'T',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bt' }),
  },
  {
    key = 'U',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bu' }),
  },
  {
    key = 'V',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bv' }),
  },
  {
    key = 'W',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bw' }),
  },
  {
    key = 'X',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bx' }),
  },
  {
    key = 'Y',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1by' }),
  },
  {
    key = 'Z',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1bz' }),
  },
  {
    key = 'A',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bA' }),
  },
  {
    key = 'B',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bB' }),
  },
  {
    key = 'C',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bC' }),
  },
  {
    key = 'D',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bD' }),
  },
  {
    key = 'E',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bE' }),
  },
  {
    key = 'F',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bF' }),
  },
  {
    key = 'G',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bG' }),
  },
  {
    key = 'H',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bH' }),
  },
  {
    key = 'I',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bI' }),
  },
  {
    key = 'J',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bJ' }),
  },
  {
    key = 'K',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bK' }),
  },
  {
    key = 'L',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bL' }),
  },
  {
    key = 'M',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bM' }),
  },
  {
    key = 'N',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bN' }),
  },
  {
    key = 'O',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bO' }),
  },
  {
    key = 'P',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bP' }),
  },
  {
    key = 'Q',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bQ' }),
  },
  {
    key = 'R',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bR' }),
  },
  {
    key = 'S',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bS' }),
  },
  {
    key = 'T',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bT' }),
  },
  {
    key = 'U',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bU' }),
  },
  {
    key = 'V',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bV' }),
  },
  {
    key = 'W',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bW' }),
  },
  {
    key = 'X',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bX' }),
  },
  {
    key = 'Y',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bY' }),
  },
  {
    key = 'Z',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1bZ' }),
  },
  {
    key = '1',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b1' }),
  },
  {
    key = '2',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b2' }),
  },
  {
    key = '3',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b3' }),
  },
  {
    key = '4',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b4' }),
  },
  {
    key = '5',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b5' }),
  },
  {
    key = '6',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b6' }),
  },
  {
    key = '7',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b7' }),
  },
  {
    key = '8',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b8' }),
  },
  {
    key = '9',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b9' }),
  },
  {
    key = '0',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b0' }),
  },
  {
    key = 'Space',
    mods = 'CTRL',
    action = wezterm.action({ SendString = '\x00' }),
  },
  {
    key = 'Grave',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b`' }),
  },
  {
    key = 'Grave',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b~' }),
  },
  {
    key = 'Period',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b.' }),
  },
  {
    key = '8',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b*' }),
  },
  {
    key = '3',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b#' }),
  },
  {
    key = 'Period',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b>' }),
  },
  {
    key = 'Comma',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b<' }),
  },
  {
    key = 'Minus',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b_' }),
  },
  {
    key = '5',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b%' }),
  },
  {
    key = '6',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b^' }),
  },
  {
    key = 'Backslash',
    mods = 'ALT',
    action = wezterm.action({ SendString = '\x1b\\' }),
  },
  {
    key = 'Backslash',
    mods = 'ALT|SHIFT',
    action = wezterm.action({ SendString = '\x1b|' }),
  },
}

-- and finally, return the configuration to wezterm
return config
