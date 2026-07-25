{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  #xdg.configFile."fish/config.fish".source = ./.config/fish/config.fish;
  xdg.configFile."fish/tide.fish".source = ./.config/fish/tide.fish;
  xdg.configFile."fish/functions/_tide_item_nohist.fish".source = ./.config/fish/functions/_tide_item_nohist.fish;
  xdg.configFile."fish/completions/nix.fish".source = "${pkgs.nix}/share/fish/vendor_completions.d/nix.fish";
  xdg.configFile."fish/completions/rustup.fish" = lib.mkIf config.dotfiles.profile.hasRustup {
    source = "${pkgs.rustup}/share/fish/vendor_completions.d/rustup.fish";
  };

  programs.fish = {
    enable = true;

    shellInit = ''
      ${builtins.readFile .config/fish/config.fish}
      # Keep mutable source-install fallbacks behind Home Manager packages.
      fish_add_path --append --path --move "$(go env GOPATH)/bin"
      fish_add_path --append --path --move "$HOME/.cargo/bin"
      set -l normalized_path

      for path_entry in $PATH
        contains -- "$path_entry" $normalized_path
        or set -a normalized_path "$path_entry"
      end

      set -gx PATH $normalized_path
      set -e normalized_path path_entry
    '';

    plugins = [
      {
        name = "autopair.fish";
        src = pkgs.fishPlugins.autopair.src;
      }
      {
        name = "puffer-fish";
        src = pkgs.fishPlugins.puffer.src;
      }
      {
        name = "spark.fish";
        src = pkgs.fishPlugins.spark.src;
      }
      {
        name = "tide";
        src = inputs.fish_tide;
      }
      {
        name = "z";
        src = inputs.fish_z;
      }
    ];
  };
}
