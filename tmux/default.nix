{pkgs, ...}: {
  programs.tmux = {
    enable = true;
    extraConfig = ''
      set -g default-shell "${pkgs.zsh}/bin/zsh"
      set -g default-command "${pkgs.fish}/bin/fish --login"

      ${builtins.readFile ./.tmux.conf}
    '';
  };
}
