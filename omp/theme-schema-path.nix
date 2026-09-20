version:
if builtins.compareVersions version "18.2.5" >= 0
then "packages/tui/src/theme/theme-schema.json"
else "packages/coding-agent/src/modes/theme/theme-schema.json"
