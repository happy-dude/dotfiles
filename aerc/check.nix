{
  homes,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};

  deployed = homes.stachan.config.home.file."${homes.stachan.config.home.homeDirectory}/.config/aerc/aerc.conf".source;
in {
  # aerc.conf exists twice: as the attribute set Home Manager deploys, because
  # it must read general.unsafe-accounts-conf from there, and as the file in
  # aerc's own format that documents the same settings. Nothing derives one
  # from the other, so this check refuses to let them disagree.
  aerc-config-mirror = mkCheck {
    name = "aerc-config-mirror";
    tools = [pkgs.python3];
    script = ''
      python3 - ${deployed} ${self}/aerc/.config/aerc/aerc.conf <<'PYTHON'
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


      deployed = settings(sys.argv[1])
      tracked = settings(sys.argv[2])

      problems = []
      for key in sorted(set(deployed) | set(tracked)):
          if deployed.get(key) != tracked.get(key):
              problems.append(
                  f"{key}\n"
                  f"    deployed: {deployed.get(key)!r}\n"
                  f"    tracked : {tracked.get(key)!r}"
              )

      if problems:
          raise SystemExit(
              "aerc/default.nix and aerc/.config/aerc/aerc.conf disagree:\n"
              + "\n".join(problems)
          )
      print(f"aerc.conf: {len(deployed)} settings match")
      PYTHON
    '';
  };
}
