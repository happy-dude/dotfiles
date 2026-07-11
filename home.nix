{
  config,
  lib,
  pkgs,
  inputs,
  username,
  ...
}: let
  solaarWrapped = config.lib.nixGL.wrap pkgs.solaar;
in {
  targets.genericLinux.nixGL.packages = inputs.nixgl.packages;
  targets.genericLinux.nixGL.defaultWrapper = "mesa";
  targets.genericLinux.nixGL.installScripts = ["mesa"];

  home = {
    inherit username;
    homeDirectory = "/home/${username}";

    sessionPath = lib.optionals (username == "schan") [
      "$HOME/.nix-profile/bin"
      "$HOME/.cargo/bin"
      "$HOME/go/bin"
      "/nix/var/nix/profiles/default/bin"
    ];

    # Change this compatibility floor only after reviewing and applying every
    # intervening Home Manager migration.
    stateVersion = "26.11";

    packages = with pkgs; [
      asmfmt
      asciinema
      autoconf
      automake
      babelfish
      bash-language-server
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
      delve
      dtc
      dust
      elfutils
      exiftool
      eza
      errcheck
      fd
      ffmpeg
      flex
      fzf
      gdb
      gettext
      ghostscript
      glibc
      gnumake
      go
      godef
      golangci-lint
      gomodifytags
      gopls
      gotags
      gotools
      hexyl
      htop
      hyperfine
      iferr
      imagemagick
      img2pdf
      impl
      jq
      less
      libgcc
      libtool
      lldb
      ltrace
      lua-language-server
      meson
      motion
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
      (perl.withPackages (ps: [
        ps.PerlLanguageServer
        ps.PerlTidy
      ]))
      pinentry-all
      pkgconf
      prettier
      procs
      qemu
      reftools
      revive
      ripgrep
      roswell
      rsync
      ruff
      rust-analyzer
      rustfmt
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
      zig
      zls
      zuban

      # Language agent: translation / dictionary / grammar / OCR / TTS tooling
      dict # DICT protocol client (needs a configured server/database, see note below)
      espeak-ng # pronunciation/TTS sanity check - voices confirmed: cmn, cmn-latn-pinyin, eo, es, it, pl, vi, vi-vn-x-south, yue, yue-latn-jyutping
      languagetool # multilingual grammar/style checker - covers eo/es/it/pl; complements aspell's spellcheck-only coverage
      ocrmypdf # OCR-to-searchable-PDF wrapper; needs tesseract5 below on PATH, does not bundle it itself
      opencc # Simplified <-> Traditional Chinese conversion (s2t/t2s/s2hk/hk2s/s2twp configs bundled)
      # python3 carrying jieba (Mandarin word segmentation) and pypinyin
      # (Pinyin generation). MUST be a withPackages wrapper, not bare
      # python3Packages.* entries — those only drop the libs in the store and
      # never become importable by a python3 on PATH. This puts an
      # `import jieba`-capable python3 on PATH, which is what language.md's
      # one-liners rely on.
      (python3.withPackages (
        ps:
          with ps; [
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
      solaarWrapped

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
      ".local/share/nix-typescript".source = "${pkgs.typescript}/lib/node_modules/typescript";
      "ros_swank".source = ./roswell/ros_swank;
    };

    sessionVariables = {
    };
  };

  xdg.configFile."autostart/solaar.desktop" = lib.mkIf (username == "schan") {
    text = ''
      [Desktop Entry]
      Type=Application
      Name=Solaar
      Comment=Logitech Unifying Receiver peripherals manager
      Exec=${solaarWrapped}/bin/solaar --window=hide
      Icon=solaar
      Terminal=false
      StartupNotify=false
      X-GNOME-UsesNotifications=true
    '';
  };

  programs.home-manager.enable = true;
}
