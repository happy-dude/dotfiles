{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  xdg.enable = true;
  xdg.mime.enable = true;
  targets.genericLinux.enable = true;

  xdg.desktopEntries.ghostty = {
    name = "Ghostty (nix)";
    type = "Application";
    comment = "A terminal emulator";
    icon = "com.mitchellh.ghostty";
    exec = "env LIBGL_ALWAYS_SOFTWARE=true ${config.lib.nixGL.wrap pkgs.ghostty}/bin/ghostty";
    categories = [
      "System"
      "TerminalEmulator"
    ];
    startupNotify = true;
    terminal = false;
    actions = {
      new-window = {
        name = "New Window";
        exec = "env LIBGL_ALWAYS_SOFTWARE=true ${config.lib.nixGL.wrap pkgs.ghostty}/bin/ghostty";
      };
    };
    settings = {
      Keywords = "terminal;tty;pty;";

      X-GNOME-UsesNotifications = "true";
      X-TerminalArgExec = "-e";
      X-TerminalArgTitle = "--title=";
      X-TerminalArgAppId = "--class=";
      X-TerminalArgDir = "--working-directory=";
      X-TerminalArgHold = "--wait-after-command";
    };
  };

  xdg.desktopEntries.ghostty-toolbox = {
    name = "Ghostty (toolbox)";
    type = "Application";
    comment = "A terminal emulator";
    icon = "com.mitchellh.ghostty";
    exec = "toolbox run --container nix-toolbox-42 ghostty";
    categories = [
      "System"
      "TerminalEmulator"
    ];
    startupNotify = true;
    terminal = false;
    actions = {
      new-window = {
        name = "New Window";
        exec = "toolbox run --container nix-toolbox-42 ghostty";
      };
    };
    settings = {
      Keywords = "terminal;tty;pty;";

      X-GNOME-UsesNotifications = "true";
      X-TerminalArgExec = "-e";
      X-TerminalArgTitle = "--title=";
      X-TerminalArgAppId = "--class=";
      X-TerminalArgDir = "--working-directory=";
      X-TerminalArgHold = "--wait-after-command";
    };
  };

  # For host configuration we need to create copy of the files, so the host system can see them
  home.activation.createHostConfig = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    desktop_entries="
      ghostty-toolbox
    "

    dest_dir="$HOME/.local/share/applications"
    src_dir="$HOME/.nix-profile/share/applications"

    # Host filesystem path (toolbox mounts host at /run/host)
    host_icon_dest="$HOME/.local/share/icons/hicolor"
    local_icon_src="$HOME/.nix-profile/share/icons/hicolor"

    # Create destination if it doesn't exist
    mkdir -p "$dest_dir"

    # Copy .desktop files
    if [ -d "$src_dir" ]; then
      for entry in $desktop_entries; do
        src="$src_dir/$entry.desktop"
        test -e "$src" || continue
        echo -e "\e[32mCreating desktop entry '$entry.desktop'\e[0m"
        cp -f "$src" "$dest_dir/"
      done
    fi

    # Copy Ghostty icons directly to host filesystem (no toolbox run needed)
    icon_sizes="16x16 32x32 128x128 256x256 512x512"
    for size in $icon_sizes; do
      src_file="$local_icon_src/$size/apps/com.mitchellh.ghostty.png"
      if [ -e "$src_file" ]; then
        mkdir -p "$host_icon_dest/$size/apps"
        cp -L "$src_file" "$host_icon_dest/$size/apps/com.mitchellh.ghostty.png" && \
          echo -e "\e[32mCopied icon $size\e[0m" || true
      fi
    done

    # Update icon cache on host
    /usr/bin/gtk-update-icon-cache -f -t "$host_icon_dest" 2>/dev/null || true
  '';
}
