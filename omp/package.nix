# oh-my-pi carrying the declared settings and none of the host's OTLP
# exporter configuration.
{
  pkgs,
  settings,
}: let
  yaml = pkgs.formats.yaml {};
  overlay = yaml.generate "omp-config.yml" settings;
in
  pkgs.symlinkJoin {
    name = "omp-dotfiles-${pkgs.omp.version}";
    paths = [pkgs.omp];
    nativeBuildInputs = [pkgs.makeWrapper];
    # The overlay is prefixed so a user's own PI_CONFIG_FILES entries load
    # after it and still win. omp initialises OTLP export whenever one of the
    # endpoint variables is set; drop the host's so a session exports nothing.
    postBuild = ''
      wrapProgram "$out/bin/omp" \
        --prefix PI_CONFIG_FILES : ${overlay} \
        --unset OTEL_EXPORTER_OTLP_ENDPOINT \
        --unset OTEL_EXPORTER_OTLP_TRACES_ENDPOINT \
        --unset OTEL_EXPORTER_OTLP_LOGS_ENDPOINT \
        --unset OTEL_EXPORTER_OTLP_METRICS_ENDPOINT \
        --unset OTEL_EXPORTER_OTLP_HEADERS \
        --unset OTEL_RESOURCE_ATTRIBUTES
    '';
  }
