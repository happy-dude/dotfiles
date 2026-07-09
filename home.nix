{
  config,
  lib,
  pkgs,
  inputs,
  username,
  ...
}:

{
  targets.genericLinux.nixGL.packages = inputs.nixgl.packages;
  targets.genericLinux.nixGL.defaultWrapper = "mesa";
  targets.genericLinux.nixGL.installScripts = [ "mesa" ];

  home = {
    inherit username;
    homeDirectory = "/home/${username}";

    stateVersion = "26.11";

    packages = with pkgs; [
      asciinema
      autoconf
      automake
      babelfish
      bash-language-server
      bat
      bear
      bc
      binutils
      bison
      bpftools
      bpftrace
      ccache
      cmake
      coccinelle
      cpio
      cscope
      ctags
      curl
      desktop-file-utils
      dex
      dtc
      dust
      elfutils
      exiftool
      eza
      fd
      ffmpeg
      flex
      fzf
      gdb
      gettext
      ghostscript
      git
      glibc
      gnumake
      go
      gopls
      hexyl
      htop
      hyperfine
      imagemagick
      img2pdf
      jq
      less
      libgcc
      libtool
      lldb
      ltrace
      lua-language-server
      meson
      moreutils
      ncurses.dev
      neovim
      ninja
      nix-prefetch-github
      nix-zsh-completions
      nixfmt
      nodejs
      openssl
      pahole
      pandoc
      pass
      patch
      perf
      (perl.withPackages (ps: [ ps.PerlLanguageServer ]))
      pinentry-all
      pkgconf
      prettier
      procs
      qemu
      ripgrep
      roswell
      rsync
      rust-analyzer
      sbcl
      shellcheck
      sparse
      stow
      strace
      stylua
      texliveFull
      tokei
      tree
      tree-sitter
      typescript-language-server
      util-linux
      valgrind
      vim
      vim-language-server
      vscode-langservers-extracted
      xclip
      xdg-utils
      xsel

      # Language agent: translation / dictionary / grammar / OCR / TTS tooling
      dict # DICT protocol client (needs a configured server/database, see note below)
      espeak-ng # pronunciation/TTS sanity check - voices confirmed: cmn, cmn-latn-pinyin, eo, es, it, pl, vi, vi-vn-x-south, yue, yue-latn-jyutping
      languagetool # multilingual grammar/style checker - covers eo/es/it/pl; complements aspell's spellcheck-only coverage
      ocrmypdf # OCR-to-searchable-PDF wrapper; needs tesseract5 below on PATH, does not bundle it itself
      opencc # Simplified <-> Traditional Chinese conversion (s2t/t2s/s2hk/hk2s/s2twp configs bundled)
      # python3 carrying Jedi Language Server plus jieba (Mandarin word
      # segmentation) + pypinyin (Pinyin generation). MUST be a withPackages
      # wrapper, not bare python3Packages.* entries — those only drop the libs
      # in the store and never become
      # importable by a python3 on PATH. This puts an `import jieba`-capable
      # python3 on PATH, which is what language.md's one-liners rely on.
      (python3.withPackages (
        ps: with ps; [
          jedi-language-server
          jieba
          pypinyin
          requests
        ]
      ))
      sdcv # StarDict console dictionary client (needs a dictionary file, see note below)
      tesseract5 # OCR engine - already bundles chi_sim/chi_tra/eng/epo/ita/pol/spa/vie traineddata, no extra config needed
      translate-shell # `trans` - MT cross-check only, never the final answer; also covers eo/es/it/pl (:eo, :es, :it, :pl)

      # Aspell spellcheck-backed word validation for Esperanto/Italian/Polish/Spanish
      aspell
      aspellDicts.eo
      aspellDicts.es
      aspellDicts.it
      aspellDicts.pl
      dictdDBs.epo2eng # only stock dictd DB found for these 4 languages; no es/it/pl dictd db in nixpkgs

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
      ".clang-format".source = ./.clang-format;
      ".editorconfig".source = ./.editorconfig;
      ".gdbinit".source = ./gdb/.gdbinit;
      ".golangci.yml".source = ./.golangci.yml;
      ".roswell/helper.el".source = ./roswell/.roswell/helper.el;
      ".stylua.toml".source = ./.stylua.toml;
      "ros_swank".source = ./roswell/ros_swank;

      # Agent prompts kept live-editable via mkOutOfStoreSymlink — it targets the
      # repo working tree, not the read-only Nix store (a plain `.source` would
      # copy them in read-only).
      ".claude/agents".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/claude/.claude/agents";
    };

    sessionVariables = {
    };
  };

  programs.home-manager.enable = true;
}
