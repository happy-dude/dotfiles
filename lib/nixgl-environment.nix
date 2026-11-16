# Environment variables that belong to a nixGL-wrapped application's graphics
# stack and must not reach the host programs it starts.
#
# config.lib.nixGL.wrap runs an application under nixGLIntel, and
# zed/default.nix adds nixVulkanIntel in front of Zed. Both wrappers export
# Nix store paths for Mesa, which a host shell or agent server would pass on
# to host programs that need the host's own libraries. ghostty/default.nix
# and zed/host-command.nix unset these names before starting such a child.
#
# Each list must hold every name its wrapper exports; the nixgl-environment
# check reads the pinned wrapper scripts and fails on a name that is missing
# here, which is how an updated nixGL input gets noticed.
{
  # nixGLIntel.
  gl = [
    "GBM_BACKENDS_PATH"
    # Not exported by the wrapper. It is Mesa's software-rendering switch,
    # which may be set for one terminal launch, for example in a virtual
    # machine without OpenGL 3.3, and is meant for that terminal alone.
    "LIBGL_ALWAYS_SOFTWARE"
    "LIBGL_DRIVERS_PATH"
    "LIBVA_DRIVERS_PATH"
    "__EGL_VENDOR_LIBRARY_FILENAMES"
    "LD_LIBRARY_PATH"
  ];

  # nixVulkanIntel.
  vulkan = [
    "VK_ICD_FILENAMES"
    "VK_LAYER_PATH"
    "LD_LIBRARY_PATH"
  ];
}
