{
  lib,
  pkgs,
  ...
}: let
  prompts = import ../agents/prompts.nix {inherit lib;};
  settings = import ./settings.nix;
  omp = import ./package.nix {inherit pkgs settings;};
  palette = import ../lib/gruvbox-material.nix;
  json = pkgs.formats.json {};
  # omp discovers task agents from ~/.omp/agent/agents/*.md, taking `name`
  # and `description` from YAML frontmatter and the rest of the file as the
  # system prompt. It skips ~/.claude/agents deliberately, because Claude's
  # frontmatter is a different contract, so the canonical prompts are
  # re-emitted here with only the fields omp defines. The description is
  # written as a JSON string, which YAML reads as a quoted scalar.
  agentFiles =
    lib.mapAttrs (
      name: prompt:
        pkgs.writeText "omp-agent-${name}.md" ''
          ---
          name: ${name}
          description: ${builtins.toJSON prompt.description}
          ---

          ${prompt.body}
        ''
    )
    prompts;
  # omp reads custom themes from ~/.omp/agent/themes/<name>.json and its
  # schema requires every colour token it draws with. Each token names a
  # palette variable, so the two variants share one role mapping and differ
  # only in `vars`.
  gruvboxMaterialTheme = name: vars: {
    "$schema" = "https://raw.githubusercontent.com/can1357/oh-my-pi/v${pkgs.omp.version}/packages/coding-agent/src/modes/theme/theme-schema.json";
    inherit name vars;
    colors = {
      accent = "aqua";
      border = "bg5";
      borderAccent = "fg0";
      borderMuted = "bg3";
      success = "green";
      error = "red";
      warning = "orange";
      muted = "gray1";
      dim = "gray0";
      text = "fg0";
      thinkingText = "gray1";
      selectedBg = "bg3";
      userMessageBg = "bg1";
      userMessageText = "fg0";
      customMessageBg = "bgStatusline2";
      customMessageText = "fg0";
      customMessageLabel = "purple";
      toolPendingBg = "bg1";
      toolSuccessBg = "visualGreen";
      toolErrorBg = "visualRed";
      toolTitle = "fg1";
      toolOutput = "gray2";
      mdHeading = "blue";
      mdLink = "aqua";
      mdLinkUrl = "green";
      mdCode = "yellow";
      mdCodeBlock = "fg0";
      mdCodeBlockBorder = "bg5";
      mdQuote = "gray1";
      mdQuoteBorder = "gray1";
      mdHr = "gray1";
      mdListBullet = "blue";
      toolDiffAdded = "green";
      toolDiffRemoved = "red";
      toolDiffContext = "gray1";
      syntaxComment = "gray1";
      syntaxKeyword = "red";
      syntaxFunction = "green";
      syntaxVariable = "blue";
      syntaxString = "yellow";
      syntaxNumber = "purple";
      syntaxType = "aqua";
      syntaxOperator = "orange";
      syntaxPunctuation = "fg0";
      thinkingOff = "gray0";
      thinkingMinimal = "gray1";
      thinkingLow = "blue";
      thinkingMedium = "aqua";
      thinkingHigh = "green";
      thinkingXhigh = "yellow";
      thinkingMax = "orange";
      bashMode = "green";
      pythonMode = "yellow";
      statusLineBg = "bgStatusline2";
      statusLineSep = "bg5";
      statusLineModel = "purple";
      statusLinePath = "blue";
      statusLineGitClean = "green";
      statusLineGitDirty = "orange";
      statusLineContext = "aqua";
      statusLineSpend = "yellow";
      statusLineStaged = "green";
      statusLineDirty = "orange";
      statusLineUntracked = "red";
      statusLineOutput = "fg0";
      statusLineCost = "yellow";
      statusLineSubagents = "purple";
    };
  };
  themes = {
    gruvbox-material = gruvboxMaterialTheme "gruvbox-material" palette.darkMedium;
    gruvbox-material-mix-dark-medium =
      gruvboxMaterialTheme "gruvbox-material-mix-dark-medium" palette.mixDarkMedium;
  };
in {
  home.packages = [omp];
  home.file =
    lib.mapAttrs' (
      name: source:
        lib.nameValuePair ".omp/agent/agents/${name}.md" {inherit source;}
    )
    agentFiles
    // lib.mapAttrs' (
      name: theme:
        lib.nameValuePair ".omp/agent/themes/${name}.json" {
          source = json.generate "omp-theme-${name}.json" theme;
        }
    )
    themes;
}
