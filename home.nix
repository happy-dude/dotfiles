{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  lessFlags = import ./lib/less-flags.nix;
  languageServers = import ./lib/language-servers.nix {inherit lib pkgs;};

  # pcre/pcre2 workarounds. nixpkgs removed C pcre and marked ocaml_pcre broken
  # in NixOS/nixpkgs#578893. Each workaround below checks the locked input for
  # the upstream fix, so a `nix flake update` that brings the fix turns the
  # workaround off and leaves a Home Manager warning to remove it.
  #
  # nixGL's wrapper check runs `${pcre}/bin/pcregrep`. pcre2grep takes the same
  # pattern and -o0, so hand nixGL.nix a pcre whose pcregrep is pcre2grep, until
  # nixGL stops taking `pcre` (nix-community/nixGL#227). nixGL.nix's defaults
  # (32-bit and Intel extensions on x86) match what its flake passes on
  # x86_64-linux.
  nixglNeedsPcreShim = (builtins.functionArgs (import "${inputs.nixgl}/nixGL.nix")) ? pcre;
  nixgl =
    if nixglNeedsPcreShim
    then
      pkgs.callPackage "${inputs.nixgl}/nixGL.nix" {
        pcre = pkgs.writeShellScriptBin "pcregrep" ''
          exec ${lib.getExe' pkgs.pcre2.bin "pcre2grep"} "$@"
        '';
      }
    else inputs.nixgl.packages;

  # Coccinelle 1.3.3 moved to pcre2 upstream (coccinelle/coccinelle@0af33438),
  # so build that against ocamlPackages.pcre2 while nixpkgs' coccinelle still
  # links ocaml_pcre (NixOS/nixpkgs#554724). Compare by name: evaluating the
  # broken ocaml_pcre's outPath would throw.
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;
  coccinelleNeedsPcre2 =
    lib.any (input: (input.name or null) == ocamlPackages.ocaml_pcre.name)
    pkgs.coccinelle.buildInputs;
  coccinelle =
    if coccinelleNeedsPcre2
    then
      pkgs.coccinelle.overrideAttrs (_: {
        version = "1.3.3";
        src = pkgs.fetchFromGitHub {
          owner = "coccinelle";
          repo = "coccinelle";
          tag = "1.3.3";
          hash = "sha256-HXrdgqtMRxEmBW+7KW/VCvpNr4bJG0gcdiVPtPX0qps=";
        };
        buildInputs = with ocamlPackages; [
          pcre2
          parmap
          pyml
          stdcompat
        ];
      })
    else pkgs.coccinelle;
in {
  warnings =
    lib.optional (!nixglNeedsPcreShim) ''
      nixGL no longer takes `pcre` (nix-community/nixGL#227 or equivalent landed);
      remove the pcregrep shim in home.nix and set
      targets.genericLinux.nixGL.packages = inputs.nixgl.packages.''
    ++ lib.optional (!coccinelleNeedsPcre2) ''
      nixpkgs' coccinelle ${pkgs.coccinelle.version} no longer links ocaml_pcre
      (NixOS/nixpkgs#554724 or equivalent landed); remove the coccinelle pcre2
      override in home.nix and list `coccinelle` with the other packages.'';
  targets.genericLinux.nixGL.packages = nixgl;
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
        alejandra # nixd formats with it in CoC and Emacs, matching `nix fmt`
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
        luaPackages.fennel
        meson
        motion
        moreutils
        (lib.lowPrio ncurses)
        ncurses.dev
        ninja
        nix-prefetch-github
        nix-zsh-completions
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
        stress-ng
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
      ++ languageServers.packages
      # treefmt with this repository's treefmt.nix, the same as `nix fmt`.
      ++ [inputs.self.formatter.${pkgs.stdenv.hostPlatform.system}]
      # The pcre2 coccinelle from the let block above.
      ++ [coccinelle];

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
