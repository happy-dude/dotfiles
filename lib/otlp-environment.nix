# OpenTelemetry variables an agent client's OTLP exporter reads.
#
# The omp and OpenCode wrappers unset these before the client starts, so a
# session that exports them for its own tracing cannot turn the client's
# exporter on or label what it sends. This is environment cleanup for one
# process, not network isolation.
[
  "OTEL_EXPORTER_OTLP_ENDPOINT"
  "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
  "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT"
  "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"
  "OTEL_EXPORTER_OTLP_HEADERS"
  "OTEL_RESOURCE_ATTRIBUTES"
]
