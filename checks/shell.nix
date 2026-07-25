{
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  scripts = mkCheck {
    name = "dotfiles-script-checks";
    tools = [
      pkgs.bash
      pkgs.fish
      pkgs.git
      pkgs.shellcheck
      pkgs.zsh
    ];
    script = ''
      for script in ${self}/scripts/*.sh; do
        bash -n "$script"
        shellcheck -x -a "$script"
      done

      for script in \
        ${self}/fish/.config/fish/*.fish \
        ${self}/fish/.config/fish/*.fish.example; do
        fish --no-execute "$script"
      done

      for script in ${self}/zsh/.zshenv ${self}/zsh/.config/zsh/.*.zsh \
        ${self}/zsh/.config/zsh/.z*; do
        if [ -f "$script" ]; then
          zsh -n "$script"
        fi
      done

      for test_script in ${self}/scripts/test_*.sh; do
        bash "$test_script"
      done
    '';
  };
}
