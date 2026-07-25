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
      script = ''
        for script in \
          ${self}/fish/.config/fish/*.fish \
          ${self}/fish/.config/fish/*.fish.example; do
          fish --no-execute "$script"
        done
      '';
    };

    zsh-syntax = mkCheck {
      name = "dotfiles-zsh-syntax";
      tools = [pkgs.zsh];
      script = ''
        for script in ${self}/zsh/.zshenv ${self}/zsh/.config/zsh/.*.zsh \
          ${self}/zsh/.config/zsh/.z*; do
          if [ -f "$script" ]; then
            zsh -n "$script"
          fi
        done
      '';
    };
  }
  // lib.listToAttrs (
    map (name: lib.nameValuePair (suiteName name) (suiteCheck name)) suites
  )
