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
      #font-family = "ComicShannsMono Nerd Font Mono";
      font-family = "FantasqueSansM Nerd Font Mono";
      font-size = 16;

      keybind = [
        # ref: https://ghostty.org/docs/config/keybind

        # use ctrl + ` as leader
        # ref: https://ghostty.org/docs/config/keybind/sequence

        # windows
        "ctrl+grave_accent>n=new_window"
        "ctrl+grave_accent>t=new_tab"

        # splits
        "ctrl+grave_accent>\\=new_split:right"
        "ctrl+grave_accent>minus=new_split:down"

        # splits: resizing
        "ctrl+grave_accent>equal=equalize_splits"
        "ctrl+grave_accent>shift+up=resize_split:up,10"
        "ctrl+grave_accent>shift+down=resize_split:down,10"
        "ctrl+grave_accent>shift+right=resize_split:right,10"
        "ctrl+grave_accent>shift+left=resize_split:left,10"

        # splits: navigation
        "ctrl+grave_accent>left_bracket=goto_split:previous"
        "ctrl+grave_accent>right_bracket=goto_split:next"
        "ctrl+grave_accent>up=goto_split:up"
        "ctrl+grave_accent>down=goto_split:down"
        "ctrl+grave_accent>right=goto_split:right"
        "ctrl+grave_accent>left=goto_split:left"

        # tabs: navigation
        "ctrl+grave_accent>one=goto_tab:1"
        "ctrl+grave_accent>two=goto_tab:2"
        "ctrl+grave_accent>three=goto_tab:3"
        "ctrl+grave_accent>four=goto_tab:4"
        "ctrl+grave_accent>five=goto_tab:5"
        "ctrl+grave_accent>six=goto_tab:6"
        "ctrl+grave_accent>seven=goto_tab:7"
        "ctrl+grave_accent>eight=goto_tab:8"
        "ctrl+grave_accent>nine=last_tab"

        # clipboard / selection
        "ctrl+grave_accent>a=select_all"
        "ctrl+grave_accent>y=copy_to_clipboard"
        "ctrl+grave_accent>p=paste_from_clipboard"

        # screen
        "ctrl+grave_accent>comma=reload_config"
        "ctrl+grave_accent>r=reset"
        "ctrl+grave_accent>l=clear_screen"

        # backspace niceties
        "alt+delete=esc:d"
        "alt+left=esc:b"
        "alt+right=esc:f"
        "super+backspace=text:\\x15"
        "super+delete=text:\\x0b"
        "super+left=text:\\x01"
        "super+right=text:\\x05"
      ];
    };
  };
}
