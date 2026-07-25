{
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  secrets = mkCheck {
    name = "dotfiles-secret-scan";
    tools = [pkgs.gitleaks];
    script = ''
      gitleaks dir --no-banner --no-color --redact ${self}
    '';
  };
}
