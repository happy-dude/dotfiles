{pkgs}: let
  inherit (pkgs) lib;
  otlpEnvironment = import ../lib/otlp-environment.nix;
in
  pkgs.symlinkJoin {
    name = "opencode-no-telemetry-${pkgs.opencode.version}";
    paths = [pkgs.opencode];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/opencode" \
        ${lib.concatMapStringsSep " " (name: "--unset ${name}") otlpEnvironment}
    '';
  }
