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

  };

  outputs =
    {
      nixpkgs,
      home-manager,
      nixgl,
      ghostty,
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
                rev = "b16e0e7391c1e5c7ba1352b32a54b058084ba9d0";
                hash = "sha256-QuUwHe7KTB5bqeI5gnLjkAjy8wKp7E7tFDrdYepqB+s=";
              };
            });
          })
        ];
      };
    in
    {
      homeConfigurations = {
        "schan" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs;
          };

          modules = [
            ./home.nix
            ./aerc
            ./emacs
            ./fish
            ./fonts
            ./ghostty
            ./nix
            ./xdg
            ./tldr
            ./tmux
            ./wezterm
            ./zsh

          ];
        };
      };
      #formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-rfc-style;
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-tree;
    };
}
