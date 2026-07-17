{
  description = "schan Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # treefmt-nix — one `nix fmt` that formats every language in the repo
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim nightly overlay
    # https://github.com/nix-community/neovim-nightly-overlay
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # ghostty
    # https://ghostty.org/docs/install/binary#nix-flake
    # https://github.com/ghostty-org/ghostty/blob/main/flake.nix
    ghostty = {
      url = "github:ghostty-org/ghostty";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Prezto includes Git submodules, which the flake lock records explicitly.
    prezto = {
      url = "git+https://github.com/sorin-ionescu/prezto?submodules=1";
      flake = false;
    };

    # nixGL
    # https://nix-community.github.io/home-manager/index.xhtml#sec-usage-gpu-non-nixos
    # https://github.com/nix-community/nixGL
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # User-scoped declarative Flatpak management on worldmind.
    nix-flatpak = {
      url = "github:gmodena/nix-flatpak?ref=v0.7.0";
    };

    # Declarative KDE Plasma preferences on worldmind.
    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    # Rust toolchains with rustc-dev, used to build RustOwl.
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Source-only Rime schema inputs. Nix flake updates advance their locked
    # revisions; Home Manager links their data into the writable Rime tree.
    rime_bopomofo = {
      url = "github:rime/rime-bopomofo";
      flake = false;
    };
    rime_cangjie = {
      url = "github:rime/rime-cangjie";
      flake = false;
    };
    rime_cantonese = {
      url = "github:rime/rime-cantonese";
      flake = false;
    };
    rime_essay = {
      url = "github:rime/rime-essay";
      flake = false;
    };
    rime_jyutping = {
      url = "github:rime/rime-jyutping";
      flake = false;
    };
    rime_loengfan = {
      url = "github:CanCLID/rime-loengfan";
      flake = false;
    };
    rime_luna_pinyin = {
      url = "github:rime/rime-luna-pinyin";
      flake = false;
    };
    rime_prelude = {
      url = "github:rime/rime-prelude";
      flake = false;
    };
    rime_stroke = {
      url = "github:rime/rime-stroke";
      flake = false;
    };
    rime_terra_pinyin = {
      url = "github:rime/rime-terra-pinyin";
      flake = false;
    };

    # Rolling Fish plugin sources.
    fish_autopair = {
      url = "github:jorgebucaran/autopair.fish";
      flake = false;
    };
    fish_puffer = {
      url = "github:nickeb96/puffer-fish";
      flake = false;
    };
    fish_spark = {
      url = "github:jorgebucaran/spark.fish";
      flake = false;
    };
    fish_tide = {
      url = "github:IlanCosman/tide";
      flake = false;
    };
    fish_z = {
      url = "github:jethrokuan/z";
      flake = false;
    };

    # Rolling source inputs consumed by local Nix modules.
    bgutil_ytdlp_pot_provider = {
      url = "github:Brainicism/bgutil-ytdlp-pot-provider";
      flake = false;
    };

    catppuccin_fcitx5 = {
      url = "github:catppuccin/fcitx5";
      flake = false;
    };

    coc_zuban = {
      url = "github:yaegassy/coc-zuban";
      flake = false;
    };

    roswell_src = {
      url = "github:roswell/roswell";
      flake = false;
    };

    virtme_ng_src = {
      url = "github:arighi/virtme-ng";
      flake = false;
    };

    rustowl_src = {
      url = "github:cordx56/rustowl?ref=v0.4.0";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    home-manager,
    ghostty,
    treefmt-nix,
    self,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
    system = "x86_64-linux";
    #system = "aarch64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        inputs.neovim-nightly-overlay.overlays.default
        inputs.rust-overlay.overlays.default
        ghostty.overlays.default
      ];
    };
    codex = import ./agents/codex.nix {inherit pkgs;};
    rimeHostFiles = import ./rime/host-files.nix {inherit pkgs;};
    rimeStateManager = import ./rime/state-manager.nix {inherit pkgs;};
    zedSettingsMaterializer = import ./zed/materializer.nix {inherit pkgs;};
    sortGitmodules =
      pkgs.writers.writePython3Bin
      "sort-gitmodules"
      {}
      (builtins.readFile ./scripts/sort_gitmodules.py);
    sortGitmodulesTest =
      pkgs.runCommand
      "sort-gitmodules-test"
      {nativeBuildInputs = [sortGitmodules];}
      ''
        printf '%s\n' \
          '[submodule "zeta"]' \
          $'\tpath = modules/zeta' \
          $'\turl = https://example.invalid/zeta' \
          '[submodule "alpha"]' \
          $'\tpath = modules/alpha' \
          $'\turl = https://example.invalid/alpha' \
          >.gitmodules

        sort-gitmodules .gitmodules
        mapfile -t sections < <(grep '^\[submodule' .gitmodules)
        [[ ''${sections[0]} == '[submodule "alpha"]' ]]
        [[ ''${sections[1]} == '[submodule "zeta"]' ]]
        before=$(sha256sum .gitmodules)
        sort-gitmodules .gitmodules
        after=$(sha256sum .gitmodules)
        [[ $before == "$after" ]]
        touch "$out"
      '';
    # Build a Home Manager config for a user, desktop, and Rime deployment.
    # The username determines /home/<username>; desktop selects session
    # integration; rimeDeployment selects Nix or legacy Stow file management.
    mkHome = {
      username,
      desktop,
      nixPackage ? pkgs.nixVersions.latest,
      rimeDeployment ? "nix",
    }:
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          inherit
            codex
            inputs
            username
            desktop
            nixPackage
            rimeDeployment
            rimeHostFiles
            rimeStateManager
            zedSettingsMaterializer
            ;
        };

        modules =
          [
            ./home.nix
            ./aerc
            ./bat
            ./agents
            ./emacs
            ./fish
            ./fonts
            ./fzf
            ./ghostty
            ./gnome
            ./git
            ./nix
            ./opencode
            ./rclone
            ./rime
            ./rime/gnome.nix
            ./roswell
            ./rustowl
            ./tldr
            ./tmux
            ./wezterm
            ./vim
            ./virtme-ng
            ./xdg
            ./yt-dlp
            ./zed
            ./zsh
          ]
          ++ lib.optionals (username == "schan") [
            inputs.nix-flatpak.homeManagerModules.nix-flatpak
            inputs.plasma-manager.homeModules.plasma-manager
            ./flatpak
            ./plasma
          ];
      };

    # One `nix fmt` for the whole repo: clang-format (C/C++), Alejandra (Nix),
    # fish_indent, shfmt, StyLua, Prettier, and Taplo.
    # shfmt/prettier honor the root .editorconfig; stylua honors
    # .stylua.toml. Submodule *contents* aren't tracked by this repo, so
    # treefmt's Git walk skips them.
    treefmtEval = treefmt-nix.lib.evalModule pkgs {
      projectRootFile = "flake.nix";
      enableDefaultExcludes = false;
      programs = {
        clang-format = {
          enable = true;
          includes = [
            "*.c"
            "*.cc"
            "*.cpp"
            "*.cxx"
            "*.h"
            "*.hh"
            "*.hpp"
            "*.hxx"
          ];
        };
        alejandra.enable = true;
        fish_indent = {
          enable = true;
          includes = [
            "*.fish"
            "*.fish.example"
          ];
        };
        shfmt = {
          enable = true;
          useEditorConfig = true;
        };
        stylua.enable = true;
        prettier = {
          enable = true;
          settings.proseWrap = "always";
        };
        ruff-format = {
          enable = true;
          lineLength = 79;
        };
        taplo.enable = true;
      };
      settings.excludes = [
        "agents/prompts/kagi-*.md" # fixed instruction budget; preserve whitespace
        "other/**" # non-managed reference configs
        "karabiner/**" # macOS + generated backups
        "rime/**/*.yaml" # input-method schemas and dictionaries (data, not code)
        "*.patch"
        "package-lock.json"
        "go.mod"
        "go.sum"
        ".gitattributes"
        ".gitignore"
        ".hgignore"
        ".svnignore"
        "*.lock"
        "LICENSE"
      ];
      settings.formatter.gitmodules = {
        command = lib.getExe sortGitmodules;
        includes = [".gitmodules"];
      };
    };
  in {
    homeConfigurations = {
      "schan" = mkHome {
        username = "schan";
        desktop = "plasma";
        nixPackage = null;
        rimeDeployment = "nix";
      };
      "stachan" = mkHome {
        username = "stachan";
        desktop = "gnome";
        rimeDeployment = "nix";
      };
    };
    packages.${system}.home-manager = home-manager.packages.${system}.home-manager;
    formatter.${system} = treefmtEval.config.build.wrapper;

    checks.${system} = {
      formatting = treefmtEval.config.build.check self;
      codex-profile-materializer = codex.checks.profileMaterializer;
      codex-agent-directory-migration = codex.checks.agentDirectoryMigration;
      gitmodules-format = sortGitmodulesTest;
      python =
        pkgs.runCommand "dotfiles-python-checks"
        {
          nativeBuildInputs = [
            pkgs.python3
            pkgs.ruff
          ];
        }
        ''
          ruff format --check --no-cache ${self}
          ruff check --no-cache ${self}
          PYTHONPYCACHEPREFIX="$TMPDIR/pycache" \
            python3 -m compileall -q ${self}
          touch "$out"
        '';
      rclone-org-watcher =
        pkgs.runCommand "rclone-org-watcher-test"
        {nativeBuildInputs = [pkgs.python3];}
        ''
          test "$(python3 ${self}/rclone/watch_org.py classify notes.org)" = sync
          test "$(python3 ${self}/rclone/watch_org.py classify org-roam.db)" = ignore
          test "$(python3 ${self}/rclone/watch_org.py classify org-roam.bak/note.org)" = ignore
          test "$(python3 ${self}/rclone/watch_org.py classify .#note.org)" = ignore
          touch "$out"
        '';
      rime-state-manager =
        pkgs.runCommand "rime-state-manager-test"
        {nativeBuildInputs = [rimeStateManager];}
        ''
          mkdir -p home source/subdir state
          printf '%s\n' owned >marker
          printf '%s\n' stamp >stamp
          printf '%s\n' schema >source/subdir/schema.yaml
          HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
            rime-state-manager claim "$PWD/marker"
          HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
            rime-state-manager deploy \
              "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
              subdir/schema.yaml
          test -L home/.local/share/fcitx5/rime/subdir/schema.yaml
          HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
            rime-state-manager release \
              "$PWD/marker" ${pkgs.coreutils}/bin/true \
              "$PWD" "$PWD/source" subdir/schema.yaml
          test ! -e home/.local/share/fcitx5/rime/subdir/schema.yaml
          touch "$out"
        '';
      rime-host-files = pkgs.runCommand "rime-host-files-test" {nativeBuildInputs = [rimeHostFiles];} ''
        source_root="$PWD/source"
        home="$PWD/home"
        state="$PWD/state"
        mkdir -p "$source_root/.config/fcitx5/conf" "$source_root/.local/share/fcitx5/themes"
        printf '%s\n' profile-v1 >"$source_root/.config/fcitx5/profile"
        printf '%s\n' classic-v1 >"$source_root/.config/fcitx5/conf/classicui.conf"
        printf '%s\n' rime-v1 >"$source_root/.config/fcitx5/conf/rime.conf"

        HOME="$home" XDG_STATE_HOME="$state" \
          rime-host-files deploy "$source_root" "$source_root/.local/share/fcitx5/themes"
        test -f "$home/.config/fcitx5/profile"
        test ! -L "$home/.config/fcitx5/profile"
        test "$(stat -c %a "$home/.config/fcitx5/profile")" = 644

        printf '%s\n' runtime-edit >"$home/.config/fcitx5/profile"
        HOME="$home" XDG_STATE_HOME="$state" \
          rime-host-files deploy "$source_root" "$source_root/.local/share/fcitx5/themes"
        grep -qx runtime-edit "$home/.config/fcitx5/profile"

        printf '%s\n' classic-v2 >"$source_root/.config/fcitx5/conf/classicui.conf"
        HOME="$home" XDG_STATE_HOME="$state" \
          rime-host-files deploy "$source_root" "$source_root/.local/share/fcitx5/themes"
        grep -qx classic-v2 "$home/.config/fcitx5/conf/classicui.conf"

        printf '%s\n' profile-v2 >"$source_root/.config/fcitx5/profile"
        if HOME="$home" XDG_STATE_HOME="$state" \
          rime-host-files deploy "$source_root" "$source_root/.local/share/fcitx5/themes"; then
          echo "accepted conflicting Rime host-file updates" >&2
          exit 1
        fi

        printf '%s\n' profile-v1 >"$source_root/.config/fcitx5/profile"
        if HOME="$home" XDG_STATE_HOME="$state" \
          rime-host-files release "$source_root" "$source_root/.local/share/fcitx5/themes"; then
          echo "discarded a runtime-modified Rime host file" >&2
          exit 1
        fi
        printf '%s\n' profile-v1 >"$home/.config/fcitx5/profile"
        HOME="$home" XDG_STATE_HOME="$state" \
          rime-host-files release "$source_root" "$source_root/.local/share/fcitx5/themes"
        test ! -e "$home/.config/fcitx5/profile"
        test ! -e "$home/.local/share/fcitx5/themes"
        touch "$out"
      '';
      zed-settings-materializer =
        pkgs.runCommand "zed-settings-materializer-test"
        {
          nativeBuildInputs = [
            pkgs.python3
            zedSettingsMaterializer
          ];
        }
        ''
          mkdir work
          printf '%s\n' \
            '{' \
            '  "theme": {"mode": "dark"},' \
            '  "vim_mode": true' \
            '}' \
            >work/static.json
          printf '%s\n' \
            '{' \
            '  // Zed accepts JSON5 comments and trailing commas.' \
            '  theme: {font_size: 14, mode: "light"},' \
            '  runtime_only: "preserved",' \
            '}' \
            >work/settings.json

          materialize-zed-settings work/static.json work/settings.json
          python3 - work/settings.json <<'PYTHON'
          import json
          import stat
          import sys
          from pathlib import Path

          path = Path(sys.argv[1])
          settings = json.loads(path.read_text(encoding="utf-8"))
          assert settings["theme"] == {"font_size": 14, "mode": "dark"}
          assert settings["runtime_only"] == "preserved"
          assert settings["vim_mode"] is True
          assert stat.S_IMODE(path.stat().st_mode) == 0o600
          PYTHON
          touch "$out"
        '';
      editor-secret-state =
        pkgs.runCommand "editor-secret-state-test"
        {
          nativeBuildInputs = [
            pkgs.neovim
            pkgs.vim
          ];
        }
        ''
          export HOME="$PWD/home"
          export DOTFILES_CACHE_VIM=${self}/vim/.vim/vimrc_dir/cache.vim
          mkdir -p "$HOME/.config/rclone" "$HOME/.config/nix"
          vim -Nu NONE -i NONE -es -S ${self}/vim/tests/secret-state.vim
          nvim --headless -u NONE -i NONE -S ${self}/vim/tests/secret-state.vim
          touch "$out"
        '';

      scripts =
        pkgs.runCommand "dotfiles-script-checks"
        {
          nativeBuildInputs = [
            pkgs.bash
            pkgs.fish
            pkgs.git
            pkgs.shellcheck
            pkgs.zsh
          ];
        }
        ''
          for script in ${self}/scripts/*.sh; do
            bash -n "$script"
            shellcheck -x -a "$script"
          done

          for script in ${self}/fish/.config/fish/*.fish ${self}/fish/.config/fish/*.fish.example; do
            fish --no-execute "$script"
          done

          for script in ${self}/zsh/.zshenv ${self}/zsh/.config/zsh/.z*; do
            if [ -f "$script" ]; then
              zsh -n "$script"
            fi
          done

          for test_script in ${self}/scripts/test_*.sh; do
            bash "$test_script"
          done

          touch "$out"
        '';

      emacs =
        pkgs.runCommand "dotfiles-emacs-checks"
        {
          nativeBuildInputs = [
            pkgs.emacs-nox
            pkgs.findutils
          ];
        }
        ''
          while IFS= read -r -d ''' file; do
            emacs --batch --quick "$file" --eval '(check-parens)'
          done < <(find ${self} -type f -name '*.el' -print0)

          while IFS= read -r -d ''' file; do
            emacs --batch --quick "$file" \
              --eval "(require 'org-lint)" \
              --eval '(let ((reports (org-lint))) (when reports (error "%s: %S" buffer-file-name reports)))'
          done < <(find ${self} -type f -name '*.org' -print0)

          touch "$out"
        '';

      neovim-org = let
        mkProfileCheck = {
          username,
          desktop,
          nixPackage ? pkgs.nixVersions.latest,
        }: let
          home = mkHome {
            inherit username desktop nixPackage;
            rimeDeployment = "nix";
          };
          parserDirectory = home.config.home.file.".local/share/nvim/site/parser".source;
          queryDirectory = home.config.home.file.".local/share/nvim/site/queries".source;
          pluginDirectory = home.config.home.file."/home/${username}/.local/share/nvim/site/pack/hm".source;
          neovim = home.config.programs.neovim.finalPackage;
          neovimConfig = pkgs.writeText "dotfiles-neovim-${username}-test.vim" ''
            ${home.config.programs.neovim.extraConfig}
          '';
        in
          pkgs.runCommand "dotfiles-neovim-org-${username}-check"
          {}
          ''
            mkdir -p data/nvim/site/pack home/cache home/state fixture
            ln -s ${parserDirectory} data/nvim/site/parser
            ln -s ${queryDirectory} data/nvim/site/queries
            ln -s ${pluginDirectory} data/nvim/site/pack/hm
            printf '%s\n' '* Parser check' > fixture/check.org

            HOME="$PWD/home" \
            XDG_CACHE_HOME="$PWD/home/cache" \
            XDG_DATA_HOME="$PWD/data" \
            XDG_STATE_HOME="$PWD/home/state" \
              ${neovim}/bin/nvim \
                --headless \
                -u ${neovimConfig} \
                fixture/check.org \
                -l ${self}/vim/tests/org.lua

            touch "$out"
          '';
        stachanCheck = mkProfileCheck {
          username = "stachan";
          desktop = "gnome";
        };
        schanCheck = mkProfileCheck {
          username = "schan";
          desktop = "plasma";
          nixPackage = null;
        };
      in
        pkgs.runCommand "dotfiles-neovim-org-check"
        {}
        ''
          test -e ${stachanCheck}
          test -e ${schanCheck}
          touch "$out"
        '';

      opencode = import ./opencode/check.nix {inherit lib mkHome pkgs;};

      workflow =
        pkgs.runCommand "dotfiles-workflow-check"
        {
          nativeBuildInputs = [
            pkgs.actionlint
            pkgs.findutils
            pkgs.pinact
          ];
        }
        ''
          find ${self}/.github/workflows -type f \
            \( -name '*.yml' -o -name '*.yaml' \) \
            -exec actionlint {} +

          cd ${self}
          pinact run --check

          touch "$out"
        '';

      rime-lua =
        pkgs.runCommand "dotfiles-rime-lua-tests"
        {
          nativeBuildInputs = [
            pkgs.findutils
            pkgs.lua
          ];
        }
        ''
          find ${self}/rime -type f -name '*.lua' -exec luac -p {} +

          cd ${self}
          lua rime/tests/cangjie5_colemak_remap.lua
          lua rime/tests/romanization.lua

          touch "$out"
        '';

      secrets =
        pkgs.runCommand "dotfiles-secret-scan"
        {
          nativeBuildInputs = [pkgs.gitleaks];
        }
        ''
          gitleaks dir --no-banner --no-color --redact ${self}
          touch "$out"
        '';
    };
  };
}
