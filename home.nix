{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  targets.genericLinux.nixGL.packages = inputs.nixgl.packages;
  targets.genericLinux.nixGL.defaultWrapper = "mesa";
  targets.genericLinux.nixGL.installScripts = [ "mesa" ];

  home = {
    username = "stanleychan";
    homeDirectory = "/home/stanleychan";

    stateVersion = "25.11";

    packages = with pkgs; [
      asciinema_3
      babelfish
      bat
      bear
      curl
      dex
      dust
      eza
      fd
      fzf
      hexyl
      htop
      hyperfine
      jq
      less
      moreutils
      neovim
      nix-prefetch-github
      pandoc
      pass
      procs
      qemu
      ripgrep
      roswell
      sbcl
      stow
      tokei
      tree
      util-linux
      vim
      xclip
      xsel
      yt-dlp

      # graphical packages
      (config.lib.nixGL.wrap mesa-demos)
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
