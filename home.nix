{
  config,
  lib,
  pkgs,
  inputs,
  username,
  ...
}: {
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

    sessionVariables = {
      BUILDKIT_PROGRESS = "plain";
      DOCKER_BUILDKIT = "1";
      EDITOR = "nvim";
      FZF_DEFAULT_COMMAND = "rg --files --hidden --follow --glob '!.git'";
      LESS = "--mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --CLEAR-SCREEN";
      MANPAGER = "nvim +Man!";
      MANWIDTH = "80";
      PAGER = "less";
      VISUAL = "nvim";
    };

    # Change this compatibility floor only after reviewing and applying every
    # intervening Home Manager migration.
    stateVersion = "26.11";

    packages = with pkgs; [
      asmfmt
      asciinema
      autoconf
      automake
      babelfish
      babashka
      bash-language-server
      bear
      bc
      binutils
      bison
      bpftools
      bpftrace
      ccache
      clojure
      clojure-lsp
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
      fennel-ls
      ffmpeg
      fish-lsp
      flex
      fnlfmt
      gdb
      gettext
      ghidra
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
      inxi
      jq
      less
      libgcc
      libtool
      lldb
      ltrace
      lua-language-server
      luaPackages.fennel
      marksman
      meson
      motion
      moreutils
      (lib.lowPrio ncurses)
      ncurses.dev
      ninja
      nix-prefetch-github
      nix-zsh-completions
      nixfmt
      nmap
      nodejs
      openssl
      p7zip
      pahole
      pandoc
      pass
      patch
      perf
      (perl.withPackages (ps: [
        ps.PerlLanguageServer
        ps.PerlTidy
      ]))
      perlnavigator
      pinentry-all
      pkgconf
      prettier
      procs
      protobuf
      # Language-agent one-liners require these libraries to be importable from
      # the python3 executable on PATH.
      (python3.withPackages (
        ps:
          with ps; [
            jieba
            pypinyin
            requests
          ]
      ))
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
      texlab
      texliveFull
      tinymist
      tokei
      traceroute
      tree
      tree-sitter
      typescript-language-server
      typst
      util-linux
      valgrind
      virtme-ng
      vim-language-server
      whois
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

      # resolve collisions for generic binaries (cc, c++, ld, etc.)
      (lib.hiPrio gcc) # gcc, g++
      (lib.lowPrio clang) # clang, clang++
      (lib.lowPrio clang-tools) # clangd
      (lib.lowPrio llvm) # llvm-ar, llvm-nm, etc.
    ];

    file = {
      ".clang-format".source = ./.clang-format;
      ".editorconfig".source = ./.editorconfig;
      ".gdbinit".source = ./gdb/gdbinit;
      ".golangci.yml".source = ./.golangci.yml;
      ".stylua.toml".source = ./.stylua.toml;
    };

    sessionVariables = {
    };
  };

  programs.home-manager.enable = true;
}
