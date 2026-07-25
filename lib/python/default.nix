{pkgs}:
pkgs.python3.pkgs.buildPythonPackage {
  pname = "dotfiles-files";
  version = "0";
  src = ./.;
  pyproject = false;

  installPhase = ''
    runHook preInstall
    install -Dm444 dotfiles_files.py \
      "$out/${pkgs.python3.sitePackages}/dotfiles_files.py"
    runHook postInstall
  '';
}
