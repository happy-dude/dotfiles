{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    package = pkgs.tmux;
    extraConfig = ''
      ${builtins.readFile ./.tmux.conf}
    '';
  };
}
