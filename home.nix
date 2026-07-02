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
      asciinema
      babelfish
      bat
      bear
      curl
      desktop-file-utils
      dex
      dust
      exiftool
      eza
      fd
      ffmpeg
      fzf
      ghostscript
      go
      hexyl
      htop
      hyperfine
      imagemagick
      img2pdf
      jq
      less
      moreutils
      neovim
      nix-prefetch-github
      nix-zsh-completions
      nixfmt
      nodejs
      ocrmypdf
      pandoc
      pass
      pinentry-all
      prettier
      procs
      qemu
      ripgrep
      roswell
      sbcl
      shellcheck
      stow
      stylua
      texliveFull
      tokei
      tree
      tree-sitter
      util-linux
      vim
      xclip
      xdg-utils
      xsel

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
      cmake
      cproto
      cscope
      ctags
      elfutils
      flex
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

      # resolve collisions for generic binaries (cc, c++, ld, etc.)
      (lib.hiPrio gcc) # gcc, g++
      (lib.lowPrio clang) # clang, clang++
      (lib.lowPrio clang-tools) # clangd
      (lib.lowPrio llvm) # llvm-ar, llvm-nm, etc.
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
