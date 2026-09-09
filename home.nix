{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  lessFlags = import ./lib/less-flags.nix;
  languageServers = import ./lib/language-servers.nix {inherit lib pkgs;};
in {
  targets.genericLinux.nixGL.packages = inputs.nixgl.packages;
  targets.genericLinux.nixGL.defaultWrapper = "mesa";
  targets.genericLinux.nixGL.installScripts = ["mesa"];

  home = {
    # Only the Nix profile bin directories belong here, and only when the host
    # owns the Nix install. ~/.cargo/bin and ~/go/bin are not Nix facts, and
    # sessionPath prepends — which would put mutable source-install binaries
    # ahead of the managed profile. The shells append them as existence-guarded
    # fallbacks instead (fish/default.nix, zsh/default.nix), keeping them behind
    # Home Manager packages, so they must not be repeated here.
    sessionPath = lib.optionals config.dotfiles.profile.hostProvidedNix [
      "$HOME/.nix-profile/bin"
      "/nix/var/nix/profiles/default/bin"
    ];

    sessionVariables = {
      BUILDKIT_PROGRESS = "plain";
      DOCKER_BUILDKIT = "1";
      EDITOR = "nvim";
      FZF_DEFAULT_COMMAND = "rg --files --hidden --follow --glob '!.git'";
      LESS = lessFlags;
      MANPAGER = "nvim +Man!";
      MANWIDTH = "80";
      PAGER = "less";
      VISUAL = "nvim";
    };

    # Change this compatibility floor only after reviewing and applying every
    # intervening Home Manager migration.
    stateVersion = "26.11";

    packages =
      (with pkgs; [
        asmfmt
        asciinema
        ast-grep
        autoconf
        automake
        babelfish
        babashka
        bear
        bc
        binutils
        bison
        bpftools
        bpftrace
        ccache
        clojure
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
        eslint
        exiftool
        eza
        errcheck
        fd
        ffmpeg
        flex
        fnlfmt
        gdb
        gettext
        gh
        ghidra
        ghostscript
        glab
        glibc
        gnumake
        go
        godef
        golangci-lint
        gomodifytags
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
        lazygit
        less
        libgcc
        libtool
        lldb
        ltrace
        luaPackages.fennel
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
        rsync
        rustfmt
        sbcl
        shellcheck
        sparse
        stow
        strace
        stylua
        texliveFull
        tokei
        traceroute
        tree
        tree-sitter
        typst
        util-linux
        valgrind
        whois
        wl-clipboard
        xclip
        xdg-utils
        xsel
        zig

        # Language agent: translation / dictionary / grammar / OCR / TTS tooling
        dict # DICT protocol client; needs a dictd server, which this repo does not declare
        espeak-ng # pronunciation/TTS sanity check - voices confirmed: cmn, cmn-latn-pinyin, eo, es, it, pl, vi, vi-vn-x-central, vi-vn-x-south, yue, yue-latn-jyutping
        languagetool # multilingual grammar/style checker - covers eo/es/it/pl; complements aspell's spellcheck-only coverage
        ocrmypdf # OCR-to-searchable-PDF wrapper; needs tesseract5 below on PATH, does not bundle it itself
        opencc # Simplified <-> Traditional Chinese conversion (s2t/t2s/s2hk/hk2s/s2twp configs bundled)
        sdcv # StarDict console dictionary client; dictionaries/default.nix points it at the built databases
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
        (config.lib.nixGL.wrap wine)

        # resolve collisions for generic binaries (cc, c++, ld, etc.)
        (lib.hiPrio gcc) # gcc, g++
        (lib.lowPrio clang) # clang, clang++
        (lib.lowPrio llvm) # llvm-ar, llvm-nm, etc.
      ])
      # clangd arrives here with clang-tools at low priority.
      ++ languageServers.packages;

    file = {
      ".clang-format".source = ./.clang-format;
      ".editorconfig".source = ./.editorconfig;
      ".golangci.yml".source = ./.golangci.yml;
    };
  };

  xdg.configFile = {
    "gdb/gdbinit".source = ./gdb/gdbinit;
    "stylua/stylua.toml".source = ./.stylua.toml;
  };

  programs.home-manager.enable = true;
}
