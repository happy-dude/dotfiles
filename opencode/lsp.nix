{pkgs}: {
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
}
