{
  description = "schan Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nixGL
    # https://nix-community.github.io/home-manager/index.xhtml#sec-usage-gpu-non-nixos
    # https://github.com/nix-community/nixGL
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim nightly overlay
    # https://github.com/nix-community/neovim-nightly-overlay
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    # ghostty
    # https://ghostty.org/docs/install/binary#nix-flake
    # https://github.com/ghostty-org/ghostty/blob/main/flake.nix
    ghostty.url = "github:ghostty-org/ghostty";

    # treefmt-nix — one `nix fmt` that formats every language in the repo
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Source-only Rime schema inputs. Nix flake updates advance their locked
    # revisions; Home Manager links their data into the writable Rime tree.
    rime_bopomofo = {
      url = "github:rime/rime-bopomofo";
      flake = false;
    };
    rime_cangjie = {
      url = "github:rime/rime-cangjie";
      flake = false;
    };
    rime_cantonese = {
      url = "github:rime/rime-cantonese";
      flake = false;
    };
    rime_essay = {
      url = "github:rime/rime-essay";
      flake = false;
    };
    rime_jyutping = {
      url = "github:rime/rime-jyutping";
      flake = false;
    };
    rime_luna_pinyin = {
      url = "github:rime/rime-luna-pinyin";
      flake = false;
    };
    rime_prelude = {
      url = "github:rime/rime-prelude";
      flake = false;
    };
    rime_stroke = {
      url = "github:rime/rime-stroke";
      flake = false;
    };
    rime_terra_pinyin = {
      url = "github:rime/rime-terra-pinyin";
      flake = false;
    };
    rime_loengfan = {
      url = "github:CanCLID/rime-loengfan";
      flake = false;
    };

    # Prezto includes Git submodules, which the flake lock records explicitly.
    prezto = {
      url = "git+https://github.com/sorin-ionescu/prezto?submodules=1";
      flake = false;
    };

    # Rolling Fish plugin sources.
    fish_autopair = {
      url = "github:jorgebucaran/autopair.fish";
      flake = false;
    };
    fish_nvm = {
      url = "github:jorgebucaran/nvm.fish";
      flake = false;
    };
    fish_puffer = {
      url = "github:nickeb96/puffer-fish";
      flake = false;
    };
    fish_spark = {
      url = "github:jorgebucaran/spark.fish";
      flake = false;
    };
    fish_tide = {
      url = "github:IlanCosman/tide";
      flake = false;
    };
    fish_z = {
      url = "github:jethrokuan/z";
      flake = false;
    };
    # Kept locked for optional activation in fish/default.nix.
    fish_sponge = {
      url = "github:meaningful-ooo/sponge";
      flake = false;
    };

  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nixgl,
      ghostty,
      treefmt-nix,
      self,
      ...
    }@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      #system = "aarch64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          inputs.neovim-nightly-overlay.overlays.default
          ghostty.overlays.default
          nixgl.overlay
          (final: prev: {
            roswell = prev.roswell.overrideAttrs (oldAttrs: rec {
              src = prev.fetchFromGitHub {
                owner = "roswell";
                repo = "roswell";
                rev = "05a2c2fa3bf1f36dc7d10786edf918ef01fcd0a7";
                hash = "sha256-ppgwclpEw17VBoVp2/o5OsX681k3uUBR912oXULz2Ow=";
              };
            });
          })
        ];
      };
      # Build a Home Manager config for a given username; home dir is
      # /home/<username>. All machines share the same modules — only the
      # username differs (e.g. schan on the personal box, stachan on work).
      mkHome =
        username:
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs username;
          };

          modules = [
            ./home.nix
            ./aerc
            ./bat
            ./emacs
            ./fish
            ./fonts
            ./ghostty
            ./git
            ./nix
            ./rime
            ./tldr
            ./tmux
            ./wezterm
            ./xdg
            ./yt-dlp
            ./zed
            ./zsh
          ];
        };

      # One `nix fmt` for the whole repo: nixfmt (.nix), shfmt (shell), stylua
      # (lua), prettier (json/md/yaml), taplo (toml). shfmt/prettier honor the
      # root .editorconfig; stylua honors .stylua.toml. Submodule *contents*
      # aren't tracked by this repo so treefmt's git walk skips them; the
      # excludes below cover tracked-but-vendored/generated trees.
      treefmtEval = treefmt-nix.lib.evalModule pkgs {
        projectRootFile = "flake.nix";
        programs = {
          nixfmt.enable = true;
          shfmt.enable = true;
          stylua.enable = true;
          prettier.enable = true;
          taplo.enable = true;
        };
        settings.global.excludes = [
          "other/**" # non-managed reference configs
          "karabiner/**" # macOS + generated backups
          "rime/**" # input-method dictionaries (data, not code)
          "*.lock"
          "LICENSE"
        ];
      };
    in
    {
      homeConfigurations = {
        "schan" = mkHome "schan"; # personal computer
        "stachan" = mkHome "stachan"; # work computer
      };
      formatter.${system} = treefmtEval.config.build.wrapper;
    };
}
