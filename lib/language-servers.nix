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
  # The server formats by running shfmt from PATH and quietly returns no
  # edits when it is missing; nixpkgs only puts shellcheck on its PATH.
  bashLanguageServer = pkgs.symlinkJoin {
    name = "bash-language-server-with-shfmt";
    paths = [pkgs.bash-language-server];
    nativeBuildInputs = [pkgs.makeBinaryWrapper];
    postBuild = ''
      wrapProgram "$out/bin/bash-language-server" \
        --suffix PATH : ${lib.makeBinPath [pkgs.shfmt]}
    '';
  };
  # The server formats and validates by running `terraform` from PATH and
  # fails each request without it. Terraform is unfree (BUSL), so offer
  # OpenTofu under that name; a host terraform earlier on PATH still wins.
  terraformCli = pkgs.runCommand "opentofu-as-terraform" {} ''
    mkdir -p "$out/bin"
    ln -s ${lib.getExe pkgs.opentofu} "$out/bin/terraform"
  '';
  terraformLanguageServer = pkgs.symlinkJoin {
    name = "terraform-ls-with-opentofu";
    paths = [pkgs.terraform-ls];
    nativeBuildInputs = [pkgs.makeBinaryWrapper];
    postBuild = ''
      wrapProgram "$out/bin/terraform-ls" \
        --suffix PATH : ${lib.makeBinPath [terraformCli]}
    '';
  };
  servers = {
    "bash-language-server" = {
      package = bashLanguageServer;
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
    # Formats Lua through `stylua --lsp`, which reads .stylua.toml.
    "stylua" = {
      package = pkgs.stylua;
      exe = "stylua";
    };
    "terraform-ls" = {
      package = terraformLanguageServer;
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
    # TypeScript 7's Go compiler; nixpkgs installs it as tsc, and it also
    # speaks LSP.
    "tsc" = {
      package = pkgs.typescript;
      exe = "tsc";
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
