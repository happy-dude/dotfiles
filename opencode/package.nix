{pkgs}: let
  inherit (pkgs) lib;
  otlpEnvironment = import ../lib/otlp-environment.nix;
  # The CLI atomically replaces its own cli.json (e.g. in the theme picker).
  # 2.2.6 merges this inline overlay over that file, without owning the file.
  cliOverlay = builtins.toJSON {
    theme.name = "gruvbox-material-mix-dark-medium";
  };
in
  pkgs.symlinkJoin {
    name = "opencode-no-telemetry-${pkgs.opencode.version}";
    paths = [pkgs.opencode];
    nativeBuildInputs = [pkgs.makeWrapper];
    passthru = {inherit cliOverlay;};
    postBuild = ''
      wrapProgram "$out/bin/opencode" \
        --set-default OPENCODE_CLI_CONFIG_CONTENT ${lib.escapeShellArg cliOverlay} \
        ${lib.concatMapStringsSep " " (name: "--unset ${name}") otlpEnvironment}
    '';
  }
