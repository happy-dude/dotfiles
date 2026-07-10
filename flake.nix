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
    bgutil_ytdlp_pot_provider = {
      url = "github:Brainicism/bgutil-ytdlp-pot-provider";
      flake = false;
    };

    rustowl_src = {
      url = "github:cordx56/rustowl";
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
    fish_nvm = {
      url = "github:jorgebucaran/nvm.fish";
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
  };

  outputs = {
    nixpkgs,
    home-manager,
    nixgl,
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
        nixgl.overlay
        (final: prev: {
          roswell = prev.roswell.overrideAttrs (_: {
            src = inputs.roswell_src;
          });
        })
      ];
    };
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
            ./emacs
            ./fish
            ./fonts
            ./ghostty
            ./git
            ./nix
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
            ./flatpak
          ];
      };

    # One `nix fmt` for the whole repo: clang-format (C/C++), Alejandra (Nix),
    # fish_indent, shfmt, StyLua, Prettier, and Taplo.
    # shfmt/prettier honor the root .editorconfig; stylua honors
    # .stylua.toml. Submodule *contents* aren't tracked by this repo, so
    # treefmt's Git walk skips them.
    treefmtEval = treefmt-nix.lib.evalModule pkgs {
      projectRootFile = "flake.nix";
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
        prettier.enable = true;
        taplo.enable = true;
      };
      settings.global.excludes = [
        "other/**" # non-managed reference configs
        "karabiner/**" # macOS + generated backups
        "rime/**/*.yaml" # input-method schemas and dictionaries (data, not code)
        "*.lock"
        "LICENSE"
      ];
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

          cp ${self}/.gitmodules .gitmodules
          bash ${self}/scripts/sort_gitmodules.sh --check
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
