{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  nixGL.packages = inputs.nixgl.packages;
  nixGL.defaultWrapper = "mesa";
  nixGL.installScripts = [ "mesa" ];

  home = {
    username = "stanleychan";
    homeDirectory = "/home/stanleychan";

    stateVersion = "24.11"; # Please read the comment before changing.

    packages = with pkgs; [
      babelfish
      bat
      bear
      curl
      dex
      dust
      eza
      fd
      fzf
      htop
      jq
      less
      moreutils
      neovim
      pandoc
      pass
      procs
      ripgrep
      roswell
      sbcl
      stow
      tokei
      tree
      vim
      xclip
      xsel
      yt-dlp

      # graphical packages
      (config.lib.nixGL.wrap glxinfo)
      (config.lib.nixGL.wrap ghostty)
    ];

    file = {
      # simple conf files
      ".config/bat/config".source = ./bat/.config/bat/config;
      ".gdbinit".source = ./gdb/.gdbinit;
      ".gitignore_global".source = ./git/.gitignore_global;

      # code
      ".clang-format".source = ./editorconfig/.clang-format;
      ".editorconfig".source = ./editorconfig/.editorconfig;
      ".golangci.yml".source = ./go/.golangci.yml;
      "ros_swank".source = ./roswell/ros_swank;
      ".roswell/helper.el".source = ./roswell/.roswell/helper.el;
      ".stylua.toml".source = ./editorconfig/.stylua.toml;
    };

    sessionVariables = {
    };
  };

  programs.home-manager.enable = true;
}
