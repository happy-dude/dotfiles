{
  inputs,
  pkgs,
  ...
}: let
  virtme-ng = pkgs.python3Packages.buildPythonApplication {
    pname = "virtme-ng";
    version = "unstable-${builtins.substring 0 8 inputs.virtme_ng_src.lastModifiedDate}";
    pyproject = true;
    src = inputs.virtme_ng_src;

    build-system = with pkgs.python3Packages; [
      argparse-manpage
      setuptools
    ];

    dependencies = with pkgs.python3Packages; [
      argcomplete
      requests
    ];

    makeWrapperArgs = [
      "--prefix"
      "PATH"
      ":"
      (pkgs.lib.makeBinPath [
        pkgs.busybox
        pkgs.openssh
        pkgs.qemu
        pkgs.socat
        pkgs.virtiofsd
      ])
    ];

    pythonImportsCheck = [
      "virtme"
      "virtme_ng"
    ];

    meta = {
      description = "Build and run kernels in a virtualized host filesystem";
      homepage = "https://github.com/arighi/virtme-ng";
      license = pkgs.lib.licenses.gpl2Only;
      mainProgram = "vng";
      platforms = pkgs.lib.platforms.linux;
    };
  };
in {
  home.packages = [virtme-ng];
}
