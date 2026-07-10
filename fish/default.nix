{
  pkgs,
  inputs,
  ...
}:

{
  #xdg.configFile."fish/config.fish".source = ./.config/fish/config.fish;
  xdg.configFile."fish/tide.fish".source = ./.config/fish/tide.fish;
  xdg.configFile."fish/completions/nix.fish".source =
    "${pkgs.nix}/share/fish/vendor_completions.d/nix.fish";

  programs.fish = {
    enable = true;

    shellInit = ''
      ${builtins.readFile .config/fish/config.fish}
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
