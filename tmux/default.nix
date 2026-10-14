{pkgs, ...}: {
  programs.tmux = {
    enable = true;
    shell = "${pkgs.zsh}/bin/zsh";
    extraConfig = ''
      ${builtins.readFile ./.tmux.conf}

      set -g default-command "${pkgs.fish}/bin/fish --login"
    '';
  };
}
