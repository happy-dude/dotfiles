# The Catppuccin Fcitx5 theme directories: one per flavor and accent.
#
# Reading the package's theme directory at evaluation time is import from
# derivation, which every IFD-free evaluation (`nix flake show`, editor
# tooling) refuses. State the names and let the rime-theme-names check hold
# them against the package at build time instead.
{lib}: let
  flavors = ["frappe" "latte" "macchiato" "mocha"];
  accents = [
    "blue"
    "flamingo"
    "green"
    "lavender"
    "maroon"
    "mauve"
    "peach"
    "pink"
    "red"
    "rosewater"
    "sapphire"
    "sky"
    "teal"
    "yellow"
  ];
in
  lib.concatMap (flavor: map (accent: "catppuccin-${flavor}-${accent}") accents) flavors
