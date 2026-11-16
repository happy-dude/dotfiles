{pkgs}: let
  inherit (pkgs) lib;
  nixglEnvironment = import ../lib/nixgl-environment.nix;
in
  pkgs.writeShellScriptBin "zed-host-command" ''
    # These paths belong to Zed's graphics stack, which runs under both
    # nixGLIntel and nixVulkanIntel, not to a host shell or ACP server.
    # Inherited overrides are discarded, not restored.
    unset ${lib.concatStringsSep " " (lib.unique (nixglEnvironment.gl ++ nixglEnvironment.vulkan))}
    exec "$@"
  ''
