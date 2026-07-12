{pkgs, ...}: let
  sharedRuntimeFiles = [
    {
      name = "after";
      path = ./.vim/after;
    }
    {
      name = "autoload";
      path = ./.vim/autoload;
    }
    {
      name = "spell";
      path = ./.vim/spell;
    }
    {
      name = "vimrc";
      path = ./.vim/vimrc;
    }
    {
      name = "vimrc_dir";
      path = ./.vim/vimrc_dir;
    }
  ];
  mkRuntime = name: extraFiles: pkgs.linkFarm name (sharedRuntimeFiles ++ extraFiles);
  vimRuntime = mkRuntime "vim-config-runtime" [];
  neovimRuntime = mkRuntime "neovim-config-runtime" [
    {
      name = "lua";
      path = ./.vim/lua;
    }
  ];
  runtimeConfig = runtime: ''
    let g:dotfiles_vim_runtime = '${runtime}'
    set runtimepath^=${runtime}
    source ${runtime}/vimrc
  '';

  cocExtensions = with pkgs.vimPlugins; [
    coc-clangd
    coc-highlight
    coc-markdownlint
    coc-rust-analyzer
    coc-vimtex
    coc-zuban
  ];

  sharedPlugins = with pkgs.vimPlugins; [
    ack-vim
    base16-vim
    coc-nvim
    csv-vim
    ctrlp-vim
    fzf-vim
    gruvbox-material
    html5-vim
    jellybeans-vim
    nerdcommenter
    rust-vim
    salt-vim
    seoul256-vim
    splitjoin-vim
    swift-vim
    undotree
    vim-airline
    vim-airline-themes
    vim-colors-solarized
    vim-dirvish
    vim-fugitive
    vim-go
    vim-gruvbox8
    vim-indent-guides
    vim-markdown
    vim-pandoc
    vim-pandoc-syntax
    vim-racket
    vim-repeat
    vim-rsi
    vim-sandwich
    vim-sexp
    vim-sexp-mappings-for-regular-people
    vim-signify
    vim-solarized8
    vim-speeddating
    vim-toml
    vim-unimpaired
    vimtex
  ];
  vimOnlyPlugins = with pkgs.vimPlugins; [
    rainbow
    vim-easymotion
  ];
  neovimOnlyPlugins = with pkgs.vimPlugins; [
    codecompanion-history-nvim
    codecompanion-nvim
    conjure
    hop-nvim
    img-clip-nvim
    neogit
    nfnl
    nvim-dap
    nvim-dap-ui
    nvim-nio
    nvim-treesitter
    nvim-treesitter-textobjects
    orgmode
    plenary-nvim
    rainbow-delimiters-nvim
    render-markdown-nvim
    which-key-nvim
  ];

  nvimTreesitter = pkgs.vimPlugins.nvim-treesitter;
  treesitterRuntime = pkgs.symlinkJoin {
    name = "nvim-treesitter-runtime";
    # Query-only languages have no grammar derivation, so include the plugin
    # runtime alongside withAllGrammars dependencies.
    paths = nvimTreesitter.withAllGrammars.passthru.dependencies ++ ["${nvimTreesitter}/runtime"];
  };
in {
  programs.vim = {
    enable = true;
    extraConfig = runtimeConfig vimRuntime;
    plugins = sharedPlugins ++ cocExtensions ++ vimOnlyPlugins;
  };

  programs.neovim = {
    enable = true;
    package = pkgs.neovim;
    extraConfig = runtimeConfig neovimRuntime;
    plugins =
      sharedPlugins
      ++ cocExtensions
      ++ neovimOnlyPlugins
      ++ [
        {
          plugin = pkgs.vimPlugins.rustowl;
          optional = true;
        }
      ];
  };

  home.file = {
    ".config/nvim/coc-settings.json".source = ./.vim/coc-settings.json;
    ".vim/coc-settings.json".source = ./.vim/coc-settings.json;
    ".local/share/nvim/site/parser".source = "${treesitterRuntime}/parser";
    ".local/share/nvim/site/queries".source = "${treesitterRuntime}/queries";
  };
}
