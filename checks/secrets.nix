{
  pkgs,
  self,
}: {
  secrets =
    pkgs.runCommand "dotfiles-secret-scan"
    {nativeBuildInputs = [pkgs.gitleaks];}
    ''
      gitleaks dir --no-banner --no-color --redact ${self}
      touch "$out"
    '';
}
