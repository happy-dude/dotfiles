{
  config,
  lib,
  pkgs,
  ...
}: let
  prompts = import ../agents/prompts.nix {inherit lib;};
  json = pkgs.formats.json {};
  opencode = pkgs.symlinkJoin {
    name = "opencode-no-telemetry-${pkgs.opencode.version}";
    paths = [pkgs.opencode];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/opencode" \
        --unset OTEL_EXPORTER_OTLP_ENDPOINT \
        --unset OTEL_EXPORTER_OTLP_HEADERS \
        --unset OTEL_RESOURCE_ATTRIBUTES
    '';
  };
  settings = {
    "$schema" = "https://opencode.ai/config.json";
    autoupdate = false;
    lsp = {
      eslint = {
        command = ["vscode-eslint-language-server" "--stdio"];
        extensions = [".ts" ".tsx" ".js" ".jsx" ".mjs" ".cjs" ".mts" ".cts" ".vue"];
      };
      fennel-ls = {
        command = ["fennel-ls" "--server"];
        extensions = [".fnl"];
      };
      fish-lsp = {
        command = ["fish-lsp" "start"];
        extensions = [".fish"];
      };
      json-ls = {
        command = ["vscode-json-language-server" "--stdio"];
        extensions = [".json" ".jsonc"];
      };
      kotlin-ls = {
        command = ["kotlin-language-server"];
        extensions = [".kt" ".kts"];
      };
      marksman = {
        command = ["marksman" "server"];
        extensions = [".md" ".markdown"];
      };
      oxlint.disabled = true;
      perlnavigator = {
        command = ["perlnavigator" "--stdio"];
        extensions = [".pl" ".pm" ".t"];
      };
      ruff = {
        command = ["ruff" "server"];
        extensions = [".py" ".pyi"];
      };
      typescript = {
        command = ["typescript-language-server" "--stdio"];
        extensions = [".ts" ".tsx" ".js" ".jsx" ".mjs" ".cjs" ".mts" ".cts"];
        initialization.tsserver.path = "${pkgs.typescript}/lib/node_modules/typescript/lib/tsserver.js";
      };
      vim-ls = {
        command = ["vim-language-server" "--stdio"];
        extensions = [".vim"];
      };
      zuban = {
        command = ["zuban" "server"];
        extensions = [".py" ".pyi"];
      };
    };
    share = "disabled";
    experimental.openTelemetry = false;
    permission = {
      bash = "ask";
      edit = "allow";
      external_directory = "ask";
      lsp = "allow";
    };
    agent =
      lib.mapAttrs (_: prompt: {
        inherit (prompt) description;
        mode = "all";
        prompt = prompt.body;
      })
      prompts;
  };
  tuiSettings = {
    "$schema" = "https://opencode.ai/tui.json";
    theme = "gruvbox-material";
  };
  gruvboxMaterialTheme = {
    "$schema" = "https://opencode.ai/theme.json";
    defs = {
      bgDim = "#1b1b1b";
      bg0 = "#282828";
      bg1 = "#32302f";
      bg2 = "#32302f";
      bg3 = "#45403d";
      bg5 = "#5a524c";
      bgStatusline2 = "#3a3735";
      fg0 = "#d4be98";
      fg1 = "#ddc7a1";
      gray0 = "#7c6f64";
      gray1 = "#928374";
      gray2 = "#a89984";
      red = "#ea6962";
      orange = "#e78a4e";
      yellow = "#d8a657";
      green = "#a9b665";
      aqua = "#89b482";
      blue = "#7daea3";
      purple = "#d3869b";
      diffRed = "#402120";
      diffGreen = "#34381b";
      diffBlue = "#0e363e";
      visualRed = "#4c3432";
      visualGreen = "#3b4439";
    };
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
in {
  home.packages = [opencode];
  home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD = "true";
  home.sessionVariables.OPENCODE_CONFIG = "${config.xdg.configHome}/opencode/local.json";
  xdg.configFile."opencode/opencode.json".source =
    json.generate "opencode.json" settings;
  xdg.configFile."opencode/tui.json".source =
    json.generate "opencode-tui.json" tuiSettings;
  xdg.configFile."opencode/themes/gruvbox-material.json".source =
    json.generate "opencode-gruvbox-material.json" gruvboxMaterialTheme;
}
