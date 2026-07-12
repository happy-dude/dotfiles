{
  config,
  lib,
  pkgs,
  ...
}: let
  filterFile = "${config.xdg.configHome}/rclone/org-bisync.filter";
  readyMarker = "${config.xdg.stateHome}/rclone/org-bisync-ready";
  workDir = "${config.xdg.cacheHome}/rclone/bisync";
  changeWatcher = pkgs.writeShellApplication {
    name = "rclone-box-org-watch";
    runtimeInputs = [
      pkgs.inotify-tools
      pkgs.systemd
    ];
    text = ''
      inotifywait \
        --monitor \
        --recursive \
        --quiet \
        --format '%w%f' \
        --event close_write,create,delete,moved_to,moved_from \
        "$HOME/org" |
        while IFS= read -r changed_path; do
          case "$changed_path" in
            "$HOME/org/org-roam.bak/"* | \
            */org-roam*.db* | \
            "$HOME/org/.dir-locals.el" | \
            */.#* | \
            *~)
              continue
              ;;
          esac

          # The fixed transient-unit name keeps the first five-minute deadline;
          # later changes join that pending batch instead of resetting it.
          systemd-run \
            --user \
            --quiet \
            --collect \
            --unit=rclone-box-org-bisync-change \
            --on-active=5m \
            systemctl --user start rclone-box-org-bisync.service \
            >/dev/null 2>&1 || true
        done
    '';
  };
in {
  home.packages = [pkgs.rclone];

  xdg.configFile."rclone/org-bisync.filter".source = ./org-bisync.filter;

  systemd.user.services.rclone-box-org-bisync = {
    Unit = {
      Description = "Synchronize Org files with Box";
      ConditionPathExists = [
        "%h/.config/rclone/rclone.conf"
        readyMarker
      ];
    };

    Service = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${lib.escapeShellArg workDir}";
      ExecStart = lib.concatStringsSep " " [
        "${lib.getExe pkgs.rclone}"
        "bisync"
        (lib.escapeShellArg "${config.home.homeDirectory}/org")
        "box:org"
        "--filter-from"
        (lib.escapeShellArg filterFile)
        "--workdir"
        (lib.escapeShellArg workDir)
        "--check-access"
        "--check-filename"
        "RCLONE_TEST"
        "--compare"
        "size,modtime,checksum"
        "--slow-hash-sync-only"
        "--create-empty-src-dirs"
        "--resilient"
        "--recover"
        "--max-lock"
        "30m"
        "--log-level"
        "INFO"
      ];
      Nice = 10;
      IOSchedulingClass = "idle";
    };
  };

  systemd.user.services.rclone-box-org-watch = {
    Unit = {
      Description = "Schedule Box synchronization after local Org changes";
      ConditionPathExists = [
        "%h/.config/rclone/rclone.conf"
        readyMarker
      ];
    };
    Service = {
      ExecStart = lib.getExe changeWatcher;
      Restart = "always";
      RestartSec = "5s";
    };
    Install.WantedBy = ["default.target"];
  };

  systemd.user.timers.rclone-box-org-bisync = {
    Unit.Description = "Synchronize Org files with Box every 15 minutes";
    Timer = {
      OnBootSec = "5m";
      OnUnitActiveSec = "15m";
      Persistent = true;
      RandomizedDelaySec = "2m";
      Unit = "rclone-box-org-bisync.service";
    };
    Install.WantedBy = ["timers.target"];
  };
}
