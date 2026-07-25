{
  description = "schan Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # treefmt-nix — one `nix fmt` that formats every language in the repo
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim nightly overlay
    # https://github.com/nix-community/neovim-nightly-overlay
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # ghostty
    # https://ghostty.org/docs/install/binary#nix-flake
    # https://github.com/ghostty-org/ghostty/blob/main/flake.nix
    ghostty = {
      url = "github:ghostty-org/ghostty";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Prezto includes Git submodules, which the flake lock records explicitly.
    prezto = {
      url = "git+https://github.com/sorin-ionescu/prezto?submodules=1";
      flake = false;
    };

    # nixGL
    # https://nix-community.github.io/home-manager/index.xhtml#sec-usage-gpu-non-nixos
    # https://github.com/nix-community/nixGL
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # User-scoped declarative Flatpak management on worldmind.
    nix-flatpak = {
      url = "github:gmodena/nix-flatpak?ref=v0.7.0";
    };

    # Declarative KDE Plasma preferences on worldmind.
    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    # Rust toolchains with rustc-dev, used to build RustOwl.
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
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
    rime_loengfan = {
      url = "github:CanCLID/rime-loengfan";
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

    # Fish plugin sources not packaged suitably in Nixpkgs.
    fish_tide = {
      url = "github:IlanCosman/tide";
      flake = false;
    };
    fish_z = {
      url = "github:jethrokuan/z";
      flake = false;
    };

    # Rolling source inputs consumed by local Nix modules.
    coc_zuban = {
      url = "github:yaegassy/coc-zuban";
      flake = false;
    };

    roswell_src = {
      url = "github:roswell/roswell";
      flake = false;
    };

    virtme_ng_src = {
      url = "github:arighi/virtme-ng";
      flake = false;
    };

    rustowl_src = {
      url = "github:cordx56/rustowl?ref=v0.4.0";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    home-manager,
    ghostty,
    treefmt-nix,
    self,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
    system = "x86_64-linux";
    #system = "aarch64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        inputs.neovim-nightly-overlay.overlays.default
        inputs.rust-overlay.overlays.default
        ghostty.overlays.default
      ];
    };
    # Build a Home Manager config for a user and desktop session.
    # The username determines /home/<username>; desktop selects integration.
    mkHome = {
      username,
      desktop,
      nixPackage ? pkgs.nixVersions.latest,
    }:
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          inherit
            inputs
            username
            desktop
            nixPackage
            ;
        };

        modules =
          [
            ./home.nix
            ./lib/profile.nix
            ./aerc
            ./bat
            ./agents
            ./emacs
            ./fish
            ./fonts
            ./fzf
            ./ghostty
            ./gnome
            ./git
            ./gpg
            ./mail
            ./nix
            ./opencode
            ./rclone
            ./rime
            ./rime/gnome.nix
            ./roswell
            ./rustowl
            ./tldr
            ./tmux
            ./wezterm
            ./vim
            ./virtme-ng
            ./xdg
            ./yt-dlp
            ./zed
            ./zsh
          ]
          ++ lib.optionals (username == "schan") [
            inputs.nix-flatpak.homeManagerModules.nix-flatpak
            inputs.plasma-manager.homeModules.plasma-manager
            ./flatpak
            ./plasma
          ];
      };
    homes = {
      schan = mkHome {
        username = "schan";
        desktop = "plasma";
        nixPackage = null;
      };
      stachan = mkHome {
        username = "stachan";
        desktop = "gnome";
      };
    };
    treefmtConfig = import ./treefmt.nix {
      inherit pkgs self treefmt-nix;
    };
    checksConfig = import ./checks {
      inherit homes lib pkgs self;
    };
  in {
    homeConfigurations = homes;
    packages.${system}.home-manager = home-manager.packages.${system}.home-manager;
    formatter.${system} = treefmtConfig.formatter;

    checks.${system} = treefmtConfig.checks // checksConfig;
  };
}
