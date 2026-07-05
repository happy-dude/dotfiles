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
