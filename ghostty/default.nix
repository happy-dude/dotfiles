{ config, pkgs, ... }:

{
  programs.ghostty = {
    enable = true;
    package = (config.lib.nixGL.wrap pkgs.ghostty);

    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    installBatSyntax = true;
    installVimSyntax = true;

    settings = {
      #command = ${pkgs.zsh} --login -c 'tmux attach -t "mux" || tmux new -s "mux"';
      #command = ${pkgs.zsh} --login;
      command = "${pkgs.fish}/bin/fish --login";

      window-width = 132;
      window-height = 48;

      theme = "GruvboxDark";
      #font-family = "Fira Code";
      #font-family = "FiraCode Nerd Font Mono";
      font-family = "ComicShannsMono Nerd Font Mono";
      font-size = 16;

      #cursor-style = "block";
      #cursor-style-blink = false;

      keybind = [
        # splits
        #"ctrl+shift+o=new_split:right"
        #"ctrl+shift+e=new_split:down"
        "ctrl+shift+|=new_split:right"
        "ctrl+shift+-=new_split:down"

        #"ctrl+shift+n="
        "alt+delete=esc:d"
        "alt+left=esc:b"
        "alt+right=esc:f"
        "super+backspace=text:\x15"
        "super+delete=text:\x0b"
        "super+left=text:\x01"
        "super+right=text:\x05"
      ];
    };
  };
}
