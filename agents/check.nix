{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  kagi-prompt-budget = mkCheck {
    name = "kagi-prompt-budget-check";
    tools = [pkgs.python3];
    script = ''
      python3 - \
        ${./prompts/kagi-kernel.md} \
        ${./prompts/kagi-language.md} <<'PYTHON'
      import sys
      from pathlib import Path

      # The Kagi assistant field accepts this many characters. Report every
      # prompt that overruns, and how much room the rest have left, so that a
      # prompt approaching the limit is visible before it crosses it.
      limit = 20_000
      report = []
      over = []
      for name in sys.argv[1:]:
          path = Path(name)
          # Store paths carry a hash prefix that obscures the file name.
          label = path.name.split("-", 1)[-1]
          length = len(path.read_text(encoding="utf-8"))
          report.append(f"{label}: {length} of {limit} characters")
          if length > limit:
              over.append(f"{label}: {length - limit} characters over")
      print("\n".join(report))
      if over:
          raise SystemExit("\n".join(over))
      PYTHON
    '';
  };
}
