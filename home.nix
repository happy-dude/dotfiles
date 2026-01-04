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
    username = "schan";
    homeDirectory = "/home/schan";

    stateVersion = "26.05";

    packages = with pkgs; [
      asciinema_3
      babelfish
      bat
      bear
      curl
      desktop-file-utils
      dex
      dust
      eza
      fd
      fzf
      go
      hexyl
      htop
      hyperfine
      jq
      less
      moreutils
      neovim
      nix-prefetch-github
      nix-zsh-completions
      nixfmt-rfc-style
      nodejs
      pandoc
      pass
      pinentry-all
      procs
      qemu
      ripgrep
      roswell
      sbcl
      stow
      tokei
      tree
      tree-sitter
      util-linux
      vim
      xclip
      xdg-utils
      xsel
      yt-dlp

      # "Development Tools"
      buildbot
      colordiff
      cvs
      cvsps
      darcs
      dejagnu
      diffstat
      doxygen
      expect
      gettext
      git
      git-annex
      git-cola
      git2cl
      gitg
      gtranslator
      highlight
      lcov
      meld
      monotone
      mr
      nemiver
      patch
      patchutils
      qgit
      quilt
      rapidsvn
      rcs
      robodoc
      scanmem
      subunit
      subversion
      tig
      tortoisehg

      # "C Development Tools and Libraries"
      astyle
      autoconf
      automake
      bison
      cbmc
      ccache
      check
      clang-tools
      cmake
      cproto
      cscope
      ctags
      elfutils
      flex
      gccgo
      gdb
      glibc
      gnumake
      indent
      libgcc
      libtool
      lldb
      ltrace
      nasm
      perf
      pkgconf
      scons
      strace
      valgrind
      yasm
      zlib
      zzuf

      # graphical packages
      (config.lib.nixGL.wrap mesa-demos)
      (config.lib.nixGL.wrap solaar)
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
