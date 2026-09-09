# Language servers shared by every editor client.
#
# home.nix installs the packages, emacs/lsp.nix takes the absolute
# executables, and the OpenCode and CoC checks supply the packages as tools,
# so a server is added or dropped in one place. The native client
# configurations (coc-settings.json, opencode.json, lsp-servers.el) still say
# which servers they use and how; this table only says where they come from.
{
  lib,
  pkgs,
}: let
  # Perl::LanguageServer runs inside the interpreter; PerlTidy formats.
  perl = pkgs.perl.withPackages (ps: [
    ps.PerlLanguageServer
    ps.PerlTidy
  ]);
  servers = {
    "bash-language-server" = {
      package = pkgs.bash-language-server;
      exe = "bash-language-server";
    };
    "clangd" = {
      package = lib.lowPrio pkgs.clang-tools;
      exe = "clangd";
    };
    "clojure-lsp" = {
      package = pkgs.clojure-lsp;
      exe = "clojure-lsp";
    };
    "fennel-ls" = {
      package = pkgs.fennel-ls;
      exe = "fennel-ls";
    };
    "fish-lsp" = {
      package = pkgs.fish-lsp;
      exe = "fish-lsp";
    };
    "gopls" = {
      package = pkgs.gopls;
      exe = "gopls";
    };
    "haskell-language-server" = {
      package = pkgs.haskell-language-server;
      exe = "haskell-language-server-wrapper";
    };
    "kotlin-language-server" = {
      package = pkgs.kotlin-language-server;
      exe = "kotlin-language-server";
    };
    "lua-language-server" = {
      package = pkgs.lua-language-server;
      exe = "lua-language-server";
    };
    "marksman" = {
      package = pkgs.marksman;
      exe = "marksman";
    };
    "nixd" = {
      package = pkgs.nixd;
      exe = "nixd";
    };
    "oxlint" = {
      package = pkgs.oxlint;
      exe = "oxlint";
    };
    "perl-language-server" = {
      package = perl;
      exe = "perl";
    };
    "perlnavigator" = {
      package = pkgs.perlnavigator;
      exe = "perlnavigator";
    };
    "ruff" = {
      package = pkgs.ruff;
      exe = "ruff";
    };
    "rust-analyzer" = {
      package = pkgs.rust-analyzer;
      exe = "rust-analyzer";
    };
    "terraform-ls" = {
      package = pkgs.terraform-ls;
      exe = "terraform-ls";
    };
    "texlab" = {
      package = pkgs.texlab;
      exe = "texlab";
    };
    "tinymist" = {
      package = pkgs.tinymist;
      exe = "tinymist";
    };
    "tsgo" = {
      package = pkgs.typescript;
      exe = "tsgo";
    };
    "vim-language-server" = {
      package = pkgs.vim-language-server;
      exe = "vim-language-server";
    };
    "vscode-eslint-language-server" = {
      package = pkgs.vscode-langservers-extracted;
      exe = "vscode-eslint-language-server";
    };
    "vscode-json-language-server" = {
      package = pkgs.vscode-langservers-extracted;
      exe = "vscode-json-language-server";
    };
    "yaml-language-server" = {
      package = pkgs.yaml-language-server;
      exe = "yaml-language-server";
    };
    "zls" = {
      package = pkgs.zls;
      exe = "zls";
    };
    "zuban" = {
      package = pkgs.zuban;
      exe = "zuban";
    };
  };
in {
  inherit servers;
  packages = lib.unique (lib.mapAttrsToList (_: server: server.package) servers);
  bin = name: "${servers.${name}.package}/bin/${servers.${name}.exe}";
}
