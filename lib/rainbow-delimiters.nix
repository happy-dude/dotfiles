# Rainbow-delimiter colours, one per nesting depth, outermost first.
#
# Neovim (rainbow-delimiters.nvim), Vim (luochen1990/rainbow) and Emacs
# (rainbow-delimiters) cycle through the list, so depth 7 reuses depth 1.
# Emacs defines faces for nine depths only, so the list holds at most nine.
# checks/rainbow-palette.nix scores it against the gruvbox-material mix
# dark-medium colours in lib/gruvbox-material.nix, the Neovim and Vim
# theme, on the #282828 background Ghostty's Gruvbox Dark theme draws
# behind their transparent one; a change that falls below that check's
# floors fails `nix flake check`. Emacs uses solarized-gruvbox-dark, whose
# text is dimmer, so the same colours stand out more there.
#
# Two things carry the separation, because thin delimiter glyphs show
# lightness better than hue:
#
# - Depths alternate between a lighter tier (1, 3, 5) and a darker one
#   (2, 4, 6), so neighbours always differ in lightness as well as hue.
# - The hues are ordered so that depths two apart are far apart on the
#   colour wheel, as in `f(g(h(x)))`.
#
# The theme has no blue or violet: its accents stop at teal (OKLCH hue 175)
# and resume at rose (hue 2). Depths 2 and 5 fill that gap at the theme's
# chroma. There is no red: lightened enough to see, the theme's red is
# indistinguishable from the aqua for deuteranopes, and darker it fails
# against the orange and yellow instead.
#
# `cterm` indexes the terminal's 16-colour palette when 'termguicolors' is
# off: bright indices for the lighter tier, normal ones for the darker. Vim
# maps colour names through its own table rather than the ANSI order, so
# DarkYellow becomes index 6 (cyan); numbers avoid that. These indices are
# checked for range but not scored: in Ghostty's Gruvbox Dark, normal blue,
# magenta and red reach only APCA Lc 23-29, and no six of its colours pass
# every floor.
[
  # The theme's yellow, unchanged.
  {
    gui = "#e9b143";
    cterm = 11;
  }
  # Blue, synthesised at hue 243.
  {
    gui = "#60a7dd";
    cterm = 4;
  }
  # The theme's aqua, lightened.
  {
    gui = "#9fd68c";
    cterm = 14;
  }
  # The theme's orange, shifted from hue 53 to 59.
  {
    gui = "#ee8400";
    cterm = 3;
  }
  # Violet, synthesised at hue 309.
  {
    gui = "#ddb7fc";
    cterm = 13;
  }
  # The theme's purple (a rose), darkened.
  {
    gui = "#c38da7";
    cterm = 5;
  }
]
