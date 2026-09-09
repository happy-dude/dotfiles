{
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};

  # One derivation per suite. A failure then names the suite that failed, an
  # unrelated edit does not re-run the others, and Nix can run them at once.
  # Discovered rather than listed, so a new suite is checked once it exists.
  suites =
    map (entry: lib.removeSuffix ".sh" (lib.removePrefix "test_" entry))
    (lib.filter (
        entry: lib.hasPrefix "test_" entry && lib.hasSuffix ".sh" entry
      )
      (builtins.attrNames (builtins.readDir "${self}/scripts")));

  suiteName = name: "test-" + lib.replaceStrings ["_"] ["-"] name;

  suiteCheck = name:
    mkCheck {
      name = "dotfiles-${suiteName name}";
      tools = [
        pkgs.bash
        pkgs.git
        # lint_commits runs the repository's commit-message linter.
        pkgs.prettier
        pkgs.python3
      ];
      script = ''bash ${self}/scripts/test_${name}.sh'';
    };
in
  {
    shellcheck = mkCheck {
      name = "dotfiles-shellcheck";
      tools = [
        pkgs.bash
        pkgs.shellcheck
      ];
      script = ''
        for script in ${self}/scripts/*.sh ${self}/scripts/lib/*.sh; do
          bash -n "$script"
          shellcheck -x -a "$script"
        done
      '';
    };

    fish-syntax = mkCheck {
      name = "dotfiles-fish-syntax";
      tools = [pkgs.fish];
      # Discover every tracked Fish file recursively; a top-level glob misses
      # functions/, conf.d/, and completions/ under fish/.config/fish.
      script = ''
        checked=0
        while IFS= read -r -d "" script; do
          fish --no-execute "$script"
          checked=$((checked + 1))
        done < <(
          find ${self}/fish -type f \
            \( -name '*.fish' -o -name '*.fish.example' \) -print0
        )
        [ "$checked" -gt 0 ] || {
          echo "no Fish files were checked" >&2
          exit 1
        }
        echo "checked $checked Fish files"
      '';
    };

    zsh-syntax = mkCheck {
      name = "dotfiles-zsh-syntax";
      tools = [pkgs.zsh];
      script = ''
        checked=0
        for script in ${self}/zsh/.config/zsh/.*.zsh \
          ${self}/zsh/.config/zsh/.z*; do
          # An unmatched glob stays a literal pattern; a matched file must
          # parse. Nothing matching at all means the files moved.
          if [ -f "$script" ]; then
            zsh -n "$script"
            checked=$((checked + 1))
          fi
        done
        [ "$checked" -gt 0 ] || {
          echo "no Zsh files were checked" >&2
          exit 1
        }
        echo "checked $checked Zsh files"
      '';
    };
  }
  // lib.listToAttrs (
    map (name: lib.nameValuePair (suiteName name) (suiteCheck name)) suites
  )
