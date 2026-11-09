{pkgs}:
pkgs.writeShellScriptBin "zed-host-command" ''
  # These paths belong to Zed's graphics stack, not a host shell or ACP server.
  # Review these names against nixGL.nix's nixGLIntel/nixVulkanIntel exports
  # when updating that input. Inherited overrides are discarded, not restored.
  unset \
    GBM_BACKENDS_PATH \
    LIBGL_ALWAYS_SOFTWARE \
    LIBGL_DRIVERS_PATH \
    LIBVA_DRIVERS_PATH \
    __EGL_VENDOR_LIBRARY_FILENAMES \
    LD_LIBRARY_PATH \
    VK_ICD_FILENAMES \
    VK_LAYER_PATH
  exec "$@"
''
