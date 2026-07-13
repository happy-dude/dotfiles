{
  description = "schan Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
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

    # nixGL
    # https://nix-community.github.io/home-manager/index.xhtml#sec-usage-gpu-non-nixos
    # https://github.com/nix-community/nixGL
    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # neovim nightly overlay
    # https://github.com/nix-community/neovim-nightly-overlay
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Rust toolchains with rustc-dev, used to build RustOwl.
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Rolling source inputs consumed by local Nix modules.
    roswell_src = {
      url = "github:roswell/roswell";
      flake = false;
    };
    virtme_ng_src = {
      url = "github:arighi/virtme-ng";
      flake = false;
    };
    bgutil_ytdlp_pot_provider = {
      url = "github:Brainicism/bgutil-ytdlp-pot-provider";
      flake = false;
    };

    rustowl_src = {
      url = "github:cordx56/rustowl?ref=v0.4.0";
      flake = false;
    };
    # ghostty
    # https://ghostty.org/docs/install/binary#nix-flake
    # https://github.com/ghostty-org/ghostty/blob/main/flake.nix
    ghostty = {
      url = "github:ghostty-org/ghostty";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # treefmt-nix — one `nix fmt` that formats every language in the repo
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
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
    rime_loengfan = {
      url = "github:CanCLID/rime-loengfan";
      flake = false;
    };

    # Prezto includes Git submodules, which the flake lock records explicitly.
    prezto = {
      url = "git+https://github.com/sorin-ionescu/prezto?submodules=1";
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

    catppuccin_fcitx5 = {
      url = "github:catppuccin/fcitx5";
      flake = false;
    };

    coc_zuban = {
      url = "github:yaegassy/coc-zuban";
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
        (final: prev: let
          cocZubanManifest = builtins.fromJSON (
            builtins.readFile "${inputs.coc_zuban}/package.json"
          );
          cocZubanPackage = final.stdenvNoCC.mkDerivation (finalAttrs: {
            pname = "coc-zuban";
            version = cocZubanManifest.version;
            src = inputs.coc_zuban;

            pnpmDeps = final.fetchPnpmDeps {
              inherit (finalAttrs) pname version src;
              pnpm = final.pnpm_10;
              fetcherVersion = 3;
              hash = "sha256-M+PGb4bQprGZjm6uZsmy80fKFJQc7lV+WOprCXWmXms=";
            };

            nativeBuildInputs = [
              final.nodejs
              final.pnpmConfigHook
              final.pnpm_10
            ];

            buildPhase = ''
              runHook preBuild
              pnpm build
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out/lib/node_modules/@yaegassy/coc-zuban"
              cp -r lib package.json LICENSE README.md \
                "$out/lib/node_modules/@yaegassy/coc-zuban/"
              runHook postInstall
            '';

            meta = {
              description = "Zuban language server extension for coc.nvim";
              homepage = "https://github.com/yaegassy/coc-zuban";
              license = final.lib.licenses.mit;
            };
          });
          rustowlManifest = builtins.fromTOML (
            builtins.readFile "${inputs.rustowl_src}/Cargo.toml"
          );
        in {
          roswell = prev.roswell.overrideAttrs (_: {
            src = inputs.roswell_src;
          });
          virtme-ng = final.python3Packages.buildPythonApplication {
            pname = "virtme-ng";
            version = "unstable-${builtins.substring 0 8 inputs.virtme_ng_src.lastModifiedDate}";
            pyproject = true;
            src = inputs.virtme_ng_src;

            build-system = with final.python3Packages; [
              argparse-manpage
              setuptools
            ];

            dependencies = with final.python3Packages; [
              argcomplete
              requests
            ];

            makeWrapperArgs = [
              "--prefix"
              "PATH"
              ":"
              (final.lib.makeBinPath [
                final.busybox
                final.openssh
                final.qemu
                final.socat
                final.virtiofsd
              ])
            ];

            pythonImportsCheck = [
              "virtme"
              "virtme_ng"
            ];

            meta = {
              description = "Build and run kernels in a virtualized host filesystem";
              homepage = "https://github.com/arighi/virtme-ng";
              license = final.lib.licenses.gpl2Only;
              mainProgram = "vng";
              platforms = final.lib.platforms.linux;
            };
          };
          vimPlugins =
            prev.vimPlugins
            // {
              coc-zuban = final.vimUtils.buildVimPlugin {
                inherit (cocZubanPackage) pname version meta;
                src = "${cocZubanPackage}/lib/node_modules/@yaegassy/coc-zuban";
              };
              rustowl = final.vimUtils.buildVimPlugin {
                pname = "rustowl-nvim";
                version = rustowlManifest.package.version;
                src = inputs.rustowl_src;

                postInstall = ''
                  find "$out" -mindepth 1 -maxdepth 1 \
                    ! -name lua ! -name ftplugin -exec rm -rf {} +
                '';
              };
              vim-sandwich = prev.vimPlugins.vim-sandwich.overrideAttrs (old: {
                meta =
                  old.meta
                  // {
                    license = {
                      free = true;
                      fullName = "NYSL 0.9982";
                      redistributable = true;
                      shortName = "NYSL";
                      url = "https://www.kmonos.net/nysl/index.en.html";
                    };
                  };
              });
              vim-solarized8 = prev.vimPlugins.vim-solarized8.overrideAttrs (old: {
                meta = old.meta // {license = final.lib.licenses.mit;};
              });
            };
        })
      ];
    };
    codexProfileMaterializer =
      pkgs.writers.writePython3Bin
      "materialize-codex-profile"
      {libraries = [pkgs.python3Packages.tomlkit];}
      ''
        import os
        import sys
        import tempfile
        from pathlib import Path

        import tomlkit
        from tomlkit.items import AoT, Table

        MANAGED_KEYS = (
            "developer_instructions",
            "model_reasoning_effort",
        )


        def load_document(path: Path, description: str):
            try:
                return tomlkit.parse(path.read_text(encoding="utf-8"))
            except (OSError, tomlkit.exceptions.ParseError) as error:
                message = f"Unable to read {description} {path}: {error}"
                raise SystemExit(message) from error


        def materialize(source: Path, target: Path) -> None:
            if not source.is_file():
                message = (
                    "Generated Codex profile is not a regular file: "
                    f"{source}"
                )
                raise SystemExit(message)

            generated = load_document(source, "generated Codex profile")
            unexpected = set(generated) - set(MANAGED_KEYS)
            if unexpected:
                names = ", ".join(sorted(unexpected))
                message = f"Generated Codex profile has unmanaged keys: {names}"
                raise SystemExit(message)
            if "developer_instructions" not in generated:
                raise SystemExit(
                    "Generated Codex profile lacks developer_instructions"
                )

            if target.is_symlink():
                raise SystemExit(f"Refusing symlinked Codex profile: {target}")
            if target.exists() and not target.is_file():
                raise SystemExit(f"Refusing non-regular Codex profile: {target}")

            if target.exists():
                runtime = load_document(target, "existing Codex profile")
            else:
                runtime = tomlkit.document()

            generated_data = generated.unwrap()
            merged = {
                key: generated_data[key]
                for key in MANAGED_KEYS
                if key in generated_data
            }
            runtime_items = [
                (key, item, item.unwrap())
                for key, item in runtime.items()
                if key not in MANAGED_KEYS
            ]
            for key, item, value in runtime_items:
                if not isinstance(item, (Table, AoT)):
                    merged[key] = value
            for key, item, value in runtime_items:
                if isinstance(item, (Table, AoT)):
                    merged[key] = value

            target.parent.mkdir(parents=True, exist_ok=True)
            descriptor, temporary_name = tempfile.mkstemp(
                dir=target.parent,
                prefix=f".{target.name}.",
            )
            temporary = Path(temporary_name)
            try:
                os.fchmod(descriptor, 0o600)
                with os.fdopen(
                    descriptor,
                    "w",
                    encoding="utf-8",
                ) as output:
                    output.write(tomlkit.dumps(merged))
                    output.flush()
                    os.fsync(output.fileno())
                os.replace(temporary, target)
                directory = os.open(
                    target.parent,
                    os.O_RDONLY | os.O_DIRECTORY,
                )
                try:
                    os.fsync(directory)
                finally:
                    os.close(directory)
            finally:
                temporary.unlink(missing_ok=True)


        if len(sys.argv) != 3:
            raise SystemExit("usage: materialize-codex-profile SOURCE TARGET")
        materialize(Path(sys.argv[1]), Path(sys.argv[2]))
      '';
    codexProfileMaterializerTest =
      pkgs.runCommand
      "codex-profile-materializer-test"
      {
        nativeBuildInputs = [
          codexProfileMaterializer
          pkgs.python3
        ];
      }
      ''
        mkdir work
        printf '%s\n' \
          'developer_instructions = "new instructions"' \
          'model_reasoning_effort = "medium"' \
          >work/generated.toml
        printf '%s\n' \
          'developer_instructions = "old instructions"' \
          'model_reasoning_effort = "low"' \
          'service_tier = "fast"' \
          "" \
          '[projects."/tmp/project"]' \
          'trust_level = "trusted"' \
          "" \
          '[tui.model_availability_nux]' \
          'model = 2' \
          >work/profile.toml

        materialize-codex-profile work/generated.toml work/profile.toml
        python3 - work/profile.toml <<'PYTHON'
        import stat
        import sys
        import tomllib
        from pathlib import Path

        path = Path(sys.argv[1])
        profile = tomllib.loads(path.read_text(encoding="utf-8"))
        assert profile["developer_instructions"] == "new instructions"
        assert profile["model_reasoning_effort"] == "medium"
        assert profile["service_tier"] == "fast"
        assert profile["projects"]["/tmp/project"]["trust_level"] == "trusted"
        assert profile["tui"]["model_availability_nux"]["model"] == 2
        assert stat.S_IMODE(path.stat().st_mode) == 0o600
        PYTHON

        before=$(sha256sum work/profile.toml)
        materialize-codex-profile work/generated.toml work/profile.toml
        after=$(sha256sum work/profile.toml)
        [[ $before == "$after" ]]

        printf '%s\n' \
          'developer_instructions = "language instructions"' \
          >work/generated.toml
        materialize-codex-profile work/generated.toml work/profile.toml
        python3 - work/profile.toml <<'PYTHON'
        import sys
        import tomllib
        from pathlib import Path

        path = Path(sys.argv[1])
        profile = tomllib.loads(path.read_text(encoding="utf-8"))
        assert profile["developer_instructions"] == "language instructions"
        assert "model_reasoning_effort" not in profile
        assert profile["service_tier"] == "fast"
        assert profile["projects"]["/tmp/project"]["trust_level"] == "trusted"
        assert profile["tui"]["model_availability_nux"]["model"] == 2
        PYTHON

        touch "$out"
      '';
    sortGitmodules = pkgs.writeShellApplication {
      name = "sort-gitmodules";
      runtimeInputs = [
        pkgs.coreutils
        pkgs.gawk
        pkgs.gnused
      ];
      text = ''
        sort_one() {
          local path=$1
          local temporary

          [[ -f $path ]] || return 0
          temporary=$(mktemp "$path.tmp.XXXXXX")
          trap 'rm -f -- "$temporary"' RETURN

          awk '
            BEGIN { block = 0; line = 0; key = "" }
            /^\[submodule/ {
              block += 1
              line = 1
              key = $2
              gsub(/("vendor\/|["\]])/, "", key)
            }
            { print key, block, line, $0; line += 1 }
          ' "$path" \
            | LC_ALL=C sort -d -f \
            | awk '{$1 = ""; $2 = ""; $3 = ""; print}' \
            | sed 's/^ *//g' \
            | awk '/^\[/ { print; next } { print "\t" $0 }' \
              >"$temporary"

          chmod --reference="$path" "$temporary"
          if cmp -s -- "$path" "$temporary"; then
            rm -f -- "$temporary"
          else
            mv -- "$temporary" "$path"
          fi
          trap - RETURN
        }

        if (($# == 0)); then
          set -- .gitmodules
        fi
        for path in "$@"; do
          sort_one "$path"
        done
      '';
    };
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
            codexProfileMaterializer
            inputs
            username
            desktop
            nixPackage
            rimeDeployment
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
            ./rclone
            ./rime
            ./rime/gnome.nix
            ./rustowl
            ./tldr
            ./tmux
            ./wezterm
            ./vim
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
      codex-profile-materializer = codexProfileMaterializerTest;
      gitmodules-format = sortGitmodulesTest;

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

          bash ${self}/scripts/test_update_submodules.sh

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
