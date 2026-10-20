{
  config,
  lib,
  pkgs,
  ...
}: let
  prompts = import ../agents/prompts.nix {inherit lib;};
  palette = import ../lib/gruvbox-material.nix;
  json = pkgs.formats.json {};
  opencode = import ./package.nix {inherit pkgs;};
  settings = {
    "$schema" = "https://opencode.ai/config.json";
    update = "disable";
    lsp = false;
    share = "disabled";
    permissions = [
      {
        action = "shell";
        resource = "*";
        effect = "ask";
      }
      {
        action = "edit";
        resource = "*";
        effect = "allow";
      }
      {
        action = "external_directory";
        resource = "*";
        effect = "ask";
      }
      {
        action = "lsp";
        resource = "*";
        effect = "allow";
      }
    ];
    agents =
      lib.mapAttrs (_: prompt: {
        inherit (prompt) description;
        mode = "all";
        system = prompt.body;
      })
      prompts;
  };
  cliSettings = {
    "$schema" = "https://opencode.ai/v2/cli.json";
    theme.name = "gruvbox-material-mix-dark-medium";
  };
  gruvboxMaterialTheme = {
    "$schema" = "https://opencode.ai/theme.json";
    defs = palette.darkMedium;
    theme = {
      primary = "blue";
      secondary = "purple";
      accent = "aqua";
      error = "red";
      warning = "orange";
      success = "green";
      info = "yellow";
      text = "fg0";
      textMuted = "gray1";
      selectedListItemText = "bg0";
      background = "bg0";
      backgroundPanel = "bg1";
      backgroundElement = "bgStatusline2";
      border = "bg5";
      borderActive = "fg0";
      borderSubtle = "bg3";
      diffAdded = "green";
      diffRemoved = "red";
      diffContext = "gray1";
      diffHunkHeader = "aqua";
      diffHighlightAdded = "green";
      diffHighlightRemoved = "red";
      diffAddedBg = "diffGreen";
      diffRemovedBg = "diffRed";
      diffContextBg = "bg1";
      diffLineNumber = "gray2";
      diffAddedLineNumberBg = "visualGreen";
      diffRemovedLineNumberBg = "visualRed";
      markdownText = "fg0";
      markdownHeading = "blue";
      markdownLink = "aqua";
      markdownLinkText = "green";
      markdownCode = "yellow";
      markdownBlockQuote = "gray1";
      markdownEmph = "purple";
      markdownStrong = "orange";
      markdownHorizontalRule = "gray1";
      markdownListItem = "blue";
      markdownListEnumeration = "aqua";
      markdownImage = "aqua";
      markdownImageText = "green";
      markdownCodeBlock = "fg0";
      syntaxComment = "gray1";
      syntaxKeyword = "red";
      syntaxFunction = "green";
      syntaxVariable = "blue";
      syntaxString = "yellow";
      syntaxNumber = "purple";
      syntaxType = "aqua";
      syntaxOperator = "orange";
      syntaxPunctuation = "fg0";
    };
  };
  gruvboxMaterialMixTheme = gruvboxMaterialTheme // {defs = palette.mixDarkMedium;};
in {
  home.packages = [opencode];
  home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD = "true";
  home.sessionVariables.OPENCODE_CONFIG = "${config.xdg.configHome}/opencode/local.json";
  # The Emacs daemon and other systemd user services never source the shell
  # session variables; environment.d is how they see the same two settings.
  systemd.user.sessionVariables = {
    OPENCODE_DISABLE_LSP_DOWNLOAD = config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD;
    OPENCODE_CONFIG = config.home.sessionVariables.OPENCODE_CONFIG;
  };
  xdg.configFile."opencode/opencode.json".source =
    json.generate "opencode.json" settings;
  xdg.configFile."opencode/cli.json".source =
    json.generate "opencode-cli.json" cliSettings;
  xdg.configFile."opencode/themes/gruvbox-material.json".source =
    json.generate "opencode-gruvbox-material.json" gruvboxMaterialTheme;
  xdg.configFile."opencode/themes/gruvbox-material-mix-dark-medium.json".source =
    json.generate "opencode-gruvbox-material-mix-dark-medium.json" gruvboxMaterialMixTheme;
}
