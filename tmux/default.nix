{pkgs, ...}: {
  programs.tmux = {
    enable = true;
    shell = "${pkgs.zsh}/bin/zsh";
    extraConfig = ''
      set -g default-command "${pkgs.fish}/bin/fish --login"

      ${builtins.readFile ./.tmux.conf}
    '';
  };
}
