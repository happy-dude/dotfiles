{
  inputs,
  pkgs,
  ...
}: let
  cocZubanManifest = builtins.fromJSON (
    builtins.readFile "${inputs.coc_zuban}/package.json"
  );
  cocZubanPackage = pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
    pname = "coc-zuban";
    version = cocZubanManifest.version;
    src = inputs.coc_zuban;

    pnpmDeps = pkgs.fetchPnpmDeps {
      inherit (finalAttrs) pname version src;
      pnpm = pkgs.pnpm_10;
      fetcherVersion = 3;
      hash = "sha256-M+PGb4bQprGZjm6uZsmy80fKFJQc7lV+WOprCXWmXms=";
    };

    nativeBuildInputs = [
      pkgs.nodejs
      pkgs.pnpmConfigHook
      pkgs.pnpm_10
    ];

    buildPhase = ''
      runHook preBuild
      pnpm build
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/lib/node_modules/@yaegassy/coc-zuban"
      cp -r lib package.json LICENSE README.md \
        "$out/lib/node_modules/@yaegassy/coc-zuban/"
      runHook postInstall
    '';

    meta = {
      description = "Zuban language server extension for coc.nvim";
      homepage = "https://github.com/yaegassy/coc-zuban";
      license = pkgs.lib.licenses.mit;
    };
  });
  cocZubanPlugin = pkgs.vimUtils.buildVimPlugin {
    inherit (cocZubanPackage) pname version meta;
    src = "${cocZubanPackage}/lib/node_modules/@yaegassy/coc-zuban";
  };
  rustowlManifest = builtins.fromTOML (
    builtins.readFile "${inputs.rustowl_src}/Cargo.toml"
  );
  rustOwlPlugin = pkgs.vimUtils.buildVimPlugin {
    pname = "rustowl-nvim";
    version = rustowlManifest.package.version;
    src = inputs.rustowl_src;

    postInstall = ''
      find "$out" -mindepth 1 -maxdepth 1 \
        ! -name lua ! -name ftplugin -exec rm -rf {} +
    '';
  };
  vimSandwichWithLicense = pkgs.vimPlugins.vim-sandwich.overrideAttrs (old: {
    meta =
      old.meta
      // {
        license = {
          free = true;
          fullName = "NYSL 0.9982";
          redistributable = true;
          shortName = "NYSL";
          url = "https://www.kmonos.net/nysl/index.en.html";
        };
      };
  });
  vimSolarized8WithLicense = pkgs.vimPlugins.vim-solarized8.overrideAttrs (old: {
    meta = old.meta // {license = pkgs.lib.licenses.mit;};
  });
  sharedRuntimeFiles = [
    {
      name = "after";
      path = ./.vim/after;
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
    coc-prettier
    coc-rust-analyzer
    cocZubanPlugin
  ];

  sharedPlugins = with pkgs.vimPlugins; [
    ack-vim
    coc-nvim
    csv-vim
    ctrlp-vim
    fzf-vim
    gruvbox-material
    html5-vim
    nerdcommenter
    rust-vim
    salt-vim
    splitjoin-vim
    swift-vim
    undotree
    vim-airline
    vim-airline-themes
    vim-dirvish
    vim-fugitive
    vim-go
    vim-gruvbox8
    vim-indent-guides
    vim-markdown
    vim-racket
    vim-repeat
    vim-rsi
    vimSandwichWithLicense
    vim-sexp
    vim-sexp-mappings-for-regular-people
    vim-signify
    vimSolarized8WithLicense
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
  baseTreesitterRuntime = pkgs.symlinkJoin {
    name = "nvim-treesitter-base-runtime";
    # Query-only languages have no grammar derivation, so include the plugin
    # runtime alongside withAllGrammars dependencies.
    paths = nvimTreesitter.withAllGrammars.passthru.dependencies ++ ["${nvimTreesitter}/runtime"];
  };
  treesitterRuntime = pkgs.runCommand "nvim-treesitter-runtime" {} ''
    mkdir -p "$out"
    cp -rs ${baseTreesitterRuntime}/. "$out/"
    chmod u+w "$out" "$out/parser"
    ln -s \
      ${pkgs.tree-sitter-grammars.tree-sitter-org-nvim}/parser \
      "$out/parser/org.so"
  '';
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
          plugin = rustOwlPlugin;
          optional = true;
        }
      ];
  };

  xdg.configFile = {
    "nvim/autoload/coc/source/codecompanion.vim".source =
      ./.vim/autoload/coc/source/codecompanion.vim;
    "nvim/coc-settings.json".source = ./.vim/coc-settings.json;
    "vim/coc-settings.json".source = ./.vim/coc-settings.json;
  };

  xdg.dataFile = {
    "nix-typescript".source = "${pkgs.typescript}/lib/node_modules/typescript";
    "nvim/site/parser".source = "${treesitterRuntime}/parser";
    "nvim/site/queries".source = "${treesitterRuntime}/queries";
  };
}
