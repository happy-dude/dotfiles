{
  homes,
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};

  # The generated file's derivation name embeds the home directory, so
  # profiles legitimately build distinct store paths; compare the contents
  # per profile instead of requiring one shared path.
  deployed =
    lib.mapAttrsToList (
      _: home:
        home.config.home.file."${home.config.home.homeDirectory}/.config/aerc/aerc.conf".source
    )
    homes;
in {
  # aerc.conf exists twice: as the attribute set Home Manager deploys, because
  # it must read general.unsafe-accounts-conf from there, and as the file in
  # aerc's own format that documents the same settings. Nothing derives one
  # from the other, so this check refuses to let them disagree.
  aerc-config-mirror = mkCheck {
    name = "aerc-config-mirror";
    tools = [pkgs.python3];
    script = ''
      python3 - ${self}/aerc/.config/aerc/aerc.conf \
          ${lib.concatMapStringsSep " " toString deployed} <<'PYTHON'
      import sys
      from pathlib import Path


      def settings(path):
          """Active settings, keyed by section and name.

          Comments and blank lines carry no configuration, and the deployed
          file is written with its own header and ordering, so only the
          settings themselves can be compared.
          """
          section = ""
          found = {}
          for line in Path(path).read_text(encoding="utf-8").splitlines():
              text = line.strip()
              if not text or text.startswith("#"):
                  continue
              if text.startswith("["):
                  section = text
                  continue
              if "=" not in text:
                  continue
              name, value = text.split("=", 1)
              found[f"{section} {name.strip()}"] = value.strip()
          return found


      tracked = settings(sys.argv[1])
      if len(sys.argv) < 3:
          raise SystemExit("no profile deploys aerc.conf")
      problems = []
      for path in sys.argv[2:]:
          deployed = settings(path)
          for key in sorted(set(deployed) | set(tracked)):
              if deployed.get(key) != tracked.get(key):
                  problems.append(
                      f"{path}: {key}\n"
                      f"    deployed: {deployed.get(key)!r}\n"
                      f"    tracked : {tracked.get(key)!r}"
                  )

      if problems:
          raise SystemExit(
              "aerc/default.nix and aerc/.config/aerc/aerc.conf disagree:\n"
              + "\n".join(problems)
          )
      print(f"aerc.conf: {len(tracked)} settings match across {len(sys.argv) - 2} profiles")
      PYTHON
    '';
  };
}
