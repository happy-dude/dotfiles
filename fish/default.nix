{
  lib,
  pkgs,
  inputs,
  username,
  ...
}: {
  #xdg.configFile."fish/config.fish".source = ./.config/fish/config.fish;
  xdg.configFile."fish/tide.fish".source = ./.config/fish/tide.fish;
  xdg.configFile."fish/completions/nix.fish".source = "${pkgs.nix}/share/fish/vendor_completions.d/nix.fish";
  xdg.configFile."fish/completions/rustup.fish" = lib.mkIf (username == "schan") {
    source = "${pkgs.rustup}/share/fish/vendor_completions.d/rustup.fish";
  };

  programs.fish = {
    enable = true;

    shellInit = ''
      ${builtins.readFile .config/fish/config.fish}
      ${lib.optionalString (username != "schan") ''
        fish_add_path -p "$(go env GOPATH)/bin"
        fish_add_path -p "$HOME/.cargo/bin"
      ''}
      ${lib.optionalString (username == "schan") ''
        set -l normalized_path

        for path_entry in $PATH
          contains -- "$path_entry" $normalized_path
          or set -a normalized_path "$path_entry"
        end

        set -gx PATH $normalized_path
        set -e normalized_path path_entry
      ''}
    '';

    plugins = [
      {
        name = "autopair.fish";
        src = inputs.fish_autopair;
      }
      {
        name = "nvm.fish";
        src = inputs.fish_nvm;
      }
      {
        name = "puffer-fish";
        src = inputs.fish_puffer;
      }
      {
        name = "spark.fish";
        src = inputs.fish_spark;
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
