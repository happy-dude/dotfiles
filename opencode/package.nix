{pkgs}:
pkgs.symlinkJoin {
  name = "opencode-no-telemetry-${pkgs.opencode.version}";
  paths = [pkgs.opencode];
  nativeBuildInputs = [pkgs.makeWrapper];
  postBuild = ''
    wrapProgram "$out/bin/opencode" \
      --unset OTEL_EXPORTER_OTLP_ENDPOINT \
      --unset OTEL_EXPORTER_OTLP_TRACES_ENDPOINT \
      --unset OTEL_EXPORTER_OTLP_LOGS_ENDPOINT \
      --unset OTEL_EXPORTER_OTLP_METRICS_ENDPOINT \
      --unset OTEL_EXPORTER_OTLP_HEADERS \
      --unset OTEL_RESOURCE_ATTRIBUTES
  '';
}
