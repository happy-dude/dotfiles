{
  homes,
  pkgs,
}: let
  mkProfileCheck = username: home: let
    parserDirectory = home.config.home.file.".local/share/nvim/site/parser".source;
    queryDirectory = home.config.home.file.".local/share/nvim/site/queries".source;
    # Home Manager registers the native package directory under an
    # absolute path, unlike the entries this module declares itself.
    pluginDirectory =
      home.config.home.file."${home.config.home.homeDirectory}/.local/share/nvim/site/pack/hm".source;
    neovim = home.config.programs.neovim.finalPackage;
    neovimConfig = pkgs.writeText "dotfiles-neovim-${username}-test.vim" ''
      ${home.config.programs.neovim.extraConfig}
    '';
  in
    pkgs.runCommand "dotfiles-neovim-org-${username}-check"
    {}
    ''
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

      touch "$out"
    '';
  stachanCheck = mkProfileCheck "stachan" homes.stachan;
  schanCheck = mkProfileCheck "schan" homes.schan;
in {
  editor-secret-state =
    pkgs.runCommand "editor-secret-state-test"
    {
      nativeBuildInputs = [
        pkgs.neovim
        pkgs.vim
      ];
    }
    ''
      export HOME="$PWD/home"
      export DOTFILES_CACHE_VIM=${./.vim/vimrc_dir/cache.vim}
      mkdir -p "$HOME/.config/rclone" "$HOME/.config/nix"
      vim -Nu NONE -i NONE -es -S ${./tests/secret-state.vim}
      nvim --headless -u NONE -i NONE -S ${./tests/secret-state.vim}
      touch "$out"
    '';
  neovim-org =
    pkgs.runCommand "dotfiles-neovim-org-check"
    {}
    ''
      test -e ${stachanCheck}
      test -e ${schanCheck}
      touch "$out"
    '';
}
