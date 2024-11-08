{
  description = "stanleychan Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim nightly overlay
    # https://github.com/nix-community/neovim-nightly-overlay
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      self,
      ...
    }@inputs:
    let
      lib = nixpkgs.lib;
      #system = "x86_64-linux";
      system = "aarch64-linux";
      pkgs = import nixpkgs { inherit system; };
      overlays = [
        inputs.neovim-nightly-overlay.overlays.default
      ];
    in
    {
      homeConfigurations = {
        "stanleychan" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          modules = [
            ./home.nix
            ./aerc
            ./emacs
            ./fonts
            ./nix
            ./tmux
            #./wezterm

            {
              nixpkgs.overlays = overlays;
            }
          ];
        };
      };
      #formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-rfc-style;
      formatter.aarch64-linux = nixpkgs.legacyPackages.aarch64-linux.nixfmt-rfc-style;
    };
}
