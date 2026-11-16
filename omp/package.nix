# oh-my-pi with declared settings and no inherited OTLP export endpoints.
{
  pkgs,
  settings,
}: let
  inherit (pkgs) lib;
  yaml = pkgs.formats.yaml {};
  overlay = yaml.generate "omp-config.yml" settings;
  otlpEnvironment = import ../lib/otlp-environment.nix;
in
  pkgs.symlinkJoin {
    name = "omp-dotfiles-${pkgs.omp.version}";
    paths = [pkgs.omp];
    nativeBuildInputs = [pkgs.makeWrapper];
    # The overlay is prefixed so a user's own PI_CONFIG_FILES entries load
    # after it and still win. omp's OTLP exporter requires an endpoint; remove
    # the inherited endpoints so the host environment cannot enable it.
    postBuild = ''
      wrapProgram "$out/bin/omp" \
        --prefix PI_CONFIG_FILES : ${overlay} \
        ${lib.concatMapStringsSep " " (name: "--unset ${name}") otlpEnvironment}
    '';
  }
