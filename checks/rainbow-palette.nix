{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  palette = import ../lib/rainbow-delimiters.nix;
  theme = (import ../lib/gruvbox-material.nix).mixDarkMedium;
  scorer = pkgs.lib.fileset.toSource {
    root = ./.;
    fileset = ./rainbow_palette.py;
  };
in {
  # Neovim and Vim set gruvbox-material's transparent background, so
  # delimiters are drawn on the terminal's own. Ghostty's Gruvbox Dark
  # theme uses the theme's bg0, #282828, so the palette is scored against
  # that, beside the theme's normal text and the function-name green that
  # follows most opening parentheses. A terminal theme with another
  # background is not scored.
  rainbow-palette = mkCheck {
    name = "dotfiles-rainbow-palette";
    tools = [pkgs.python3];
    script = ''
      PYTHONPATH=${scorer} python3 ${./test_rainbow_palette.py}
      python3 ${./rainbow_palette.py} \
        ${pkgs.writeText "rainbow-delimiters.json" (builtins.toJSON palette)} \
        '${theme.bg0}' '${theme.fg0}' '${theme.green}'
    '';
  };
}
