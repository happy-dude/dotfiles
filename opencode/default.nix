{
  config,
  lib,
  pkgs,
  ...
}: let
  prompts = import ../agents/prompts.nix {inherit lib;};
  json = pkgs.formats.json {};
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
in {
  home.packages = [pkgs.opencode];
  home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD = "true";
  home.sessionVariables.OPENCODE_CONFIG = "${config.xdg.configHome}/opencode/local.json";
  xdg.configFile."opencode/opencode.json".source =
    json.generate "opencode.json" settings;
}
