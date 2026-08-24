{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  kagi-prompt-budget = mkCheck {
    name = "kagi-prompt-budget-check";
    tools = [pkgs.python3];
    script = ''
      python3 - ${./prompts} <<'PYTHON'
      import sys
      from pathlib import Path

      # The Kagi assistant field accepts this many characters. Report every
      # prompt that overruns, and how much room the rest have left, so that a
      # prompt approaching the limit is visible before it crosses it.
      limit = 20_000
      report = []
      over = []
      for path in sorted(Path(sys.argv[1]).glob("kagi-*.md")):
          label = path.name
          length = len(path.read_text(encoding="utf-8"))
          report.append(f"{label}: {length} of {limit} characters")
          if length > limit:
              over.append(f"{label}: {length - limit} characters over")
      if not report:
          raise SystemExit("no kagi-*.md prompts found")
      print("\n".join(report))
      if over:
          raise SystemExit("\n".join(over))
      PYTHON
    '';
  };
}
