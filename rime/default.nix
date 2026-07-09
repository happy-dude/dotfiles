{ lib, ... }:

let
  rimeDataDir = ./.local/share/fcitx5/rime;

  filesRecursively = dir:
    let
      entries = builtins.readDir dir;
    in
    lib.concatMap (
      name:
      let
        path = dir + "/${name}";
      in
      if entries.${name} == "directory" then filesRecursively path else [ path ]
    ) (builtins.attrNames entries);

  rimeDataFiles = filesRecursively rimeDataDir;
  relativeToRimeData = path: lib.removePrefix "${toString rimeDataDir}/" (toString path);
in
{
  xdg = {
    configFile = {
      "fcitx5/profile".source = ./.config/fcitx5/profile;
      "fcitx5/conf/classicui.conf".source = ./.config/fcitx5/conf/classicui.conf;
      "fcitx5/conf/rime.conf".source = ./.config/fcitx5/conf/rime.conf;
    };

    # Link static files individually so Rime can create its writable build and
    # user-data directories beside them.
    dataFile = {
      "fcitx5/themes".source = ./.local/share/fcitx5/themes;
    }
    // lib.listToAttrs (
      map (
        path: {
          name = "fcitx5/rime/${relativeToRimeData path}";
          value.source = path;
        }
      ) rimeDataFiles
    );
  };
}
