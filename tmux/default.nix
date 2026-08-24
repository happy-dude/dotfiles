{pkgs, ...}: {
  programs.tmux = {
    enable = true;
    package = pkgs.tmux;
    extraConfig = ''
      set -gu default-shell
      set -g default-shell "${pkgs.zsh}/bin/zsh"

      set -gu default-command
      set -g default-command "${pkgs.fish}/bin/fish --login"

      ${builtins.readFile ./.tmux.conf}
    '';
  };
}
