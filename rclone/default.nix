{
  config,
  lib,
  pkgs,
  ...
}: let
  filterFile = "${config.xdg.configHome}/rclone/org-bisync.filter";
  readyMarker = "${config.xdg.stateHome}/rclone/org-bisync-ready";
  workDir = "${config.xdg.cacheHome}/rclone/bisync";
  changeWatcher = import ../lib/python/mkScript.nix {inherit pkgs;} {
    name = "rclone-box-org-watch";
    source = ./watch_org.py;
  };
  watcherCommand = lib.concatStringsSep " " [
    (lib.getExe changeWatcher)
    "watch"
    (lib.escapeShellArg "${config.home.homeDirectory}/org")
    (lib.escapeShellArg "${pkgs.inotify-tools}/bin/inotifywait")
    (lib.escapeShellArg "${pkgs.systemd}/bin/systemd-run")
    (lib.escapeShellArg "${pkgs.systemd}/bin/systemctl")
  ];
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
        "--max-delete"
        "50"
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
      ExecStart = watcherCommand;
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
