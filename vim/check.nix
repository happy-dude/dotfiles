{
  homes,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  mkProfileCheck = username: home: let
    parserDirectory = home.config.xdg.dataFile."nvim/site/parser".source;
    queryDirectory = home.config.xdg.dataFile."nvim/site/queries".source;
    # Home Manager registers the native package directory under an
    # absolute path, unlike the entries this module declares itself.
    pluginDirectory =
      home.config.home.file."${home.config.home.homeDirectory}/.local/share/nvim/site/pack/hm".source;
    neovim = home.config.programs.neovim.finalPackage;
    neovimConfig = pkgs.writeText "dotfiles-neovim-${username}-test.vim" ''
      ${home.config.programs.neovim.extraConfig}
    '';
  in
    mkCheck {
      name = "dotfiles-neovim-org-${username}-check";
      script = ''
        mkdir -p data/nvim/site/pack home/cache home/state fixture
        ln -s ${parserDirectory} data/nvim/site/parser
        ln -s ${queryDirectory} data/nvim/site/queries
        ln -s ${pluginDirectory} data/nvim/site/pack/hm
        printf '%s\n' '* Parser check' > fixture/check.org

        HOME="$PWD/home" \
        XDG_CACHE_HOME="$PWD/home/cache" \
        XDG_DATA_HOME="$PWD/data" \
        XDG_STATE_HOME="$PWD/home/state" \
          ${neovim}/bin/nvim \
            --headless \
            -u ${neovimConfig} \
            fixture/check.org \
            -l ${./tests/org.lua}

        HOME="$PWD/home" \
        XDG_CACHE_HOME="$PWD/home/cache" \
        XDG_DATA_HOME="$PWD/data" \
        XDG_STATE_HOME="$PWD/home/state" \
          ${neovim}/bin/nvim \
            --headless \
            -u ${neovimConfig} \
            -l ${./tests/codecompanion.lua}

        # matchtag.lua locates the repository from its own store path and
        # reads parsers from MATCHTAG_TREESITTER_RUNTIME.
        MATCHTAG_TREESITTER_RUNTIME="$PWD/data/nvim/site" \
        HOME="$PWD/home" \
          ${neovim}/bin/nvim \
            --headless \
            -u NONE \
            -i NONE \
            -l ${self}/vim/tests/matchtag.lua

      '';
    };
  stachanCheck = mkProfileCheck "stachan" homes.stachan;
  schanCheck = mkProfileCheck "schan" homes.schan;
in {
  # coc-settings.json drives its language servers by bare command; assert each
  # command it names resolves to an installed language-server package. The
  # servers are supplied as tools rather than the whole profile: building the
  # full home.path (tens of GB) would make `nix flake check` run out of disk on
  # CI, which only evaluates the profiles on push. Keep this list in step with
  # the servers coc-settings.json configures.
  coc-language-servers = mkCheck {
    name = "coc-language-servers";
    tools = with pkgs; [
      jq
      bash-language-server
      clojure-lsp
      fennel-ls
      fish-lsp
      gopls
      haskell-language-server
      kotlin-language-server
      lua-language-server
      marksman
      nixd
      oxlint
      perl
      perlnavigator
      ruff
      terraform-ls
      texlab
      tinymist
      typescript-language-server
      vscode-langservers-extracted
      vim-language-server
      yaml-language-server
      zls
    ];
    script = ''
      missing=""
      for cmd in $(jq -r '.languageserver | to_entries[] | .value.command' \
          ${./.vim/coc-settings.json} | sort -u); do
        command -v "$cmd" >/dev/null || missing="$missing $cmd"
      done
      [ -z "$missing" ] || {
        echo "coc-settings.json names servers with no matching package:$missing" >&2
        exit 1
      }
      echo "all coc-settings.json language servers resolve"
    '';
  };
  editor-search-highlight = mkCheck {
    name = "editor-search-highlight-test";
    tools = [
      pkgs.neovim
      pkgs.vim
    ];
    script = ''
      # search.vim locates the repository from its own store path.
      export HOME="$PWD/home"
      mkdir -p "$HOME"
      vim -Nu NONE -i NONE -es -S ${self}/vim/tests/search.vim
      nvim --headless -u NONE -i NONE -S ${self}/vim/tests/search.vim
    '';
  };
  editor-secret-state = mkCheck {
    name = "editor-secret-state-test";
    tools = [
      pkgs.neovim
      pkgs.vim
    ];
    script = ''
      export HOME="$PWD/home"
      export DOTFILES_CACHE_VIM=${./.vim/vimrc_dir/cache.vim}
      mkdir -p "$HOME/.config/rclone" "$HOME/.config/nix"
      vim -Nu NONE -i NONE -es -S ${./tests/secret-state.vim}
      nvim --headless -u NONE -i NONE -S ${./tests/secret-state.vim}
    '';
  };
  neovim-org = mkCheck {
    name = "dotfiles-neovim-org-check";
    script = ''
      test -e ${stachanCheck}
      test -e ${schanCheck}
    '';
  };
}
