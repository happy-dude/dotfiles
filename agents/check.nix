{pkgs}: {
  kagi-prompt-budget =
    pkgs.runCommand "kagi-prompt-budget-check"
    {nativeBuildInputs = [pkgs.python3];}
    ''
      python3 - \
        ${./prompts/kagi-kernel.md} \
        ${./prompts/kagi-language.md} <<'PYTHON'
      import sys
      from pathlib import Path

      limit = 20_000
      for name in sys.argv[1:]:
          path = Path(name)
          length = len(path.read_text(encoding="utf-8"))
          if length > limit:
              raise SystemExit(f"{path.name}: {length} characters exceeds {limit}")
      PYTHON
      touch "$out"
    '';
}
