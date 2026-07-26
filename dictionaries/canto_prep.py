"""Fold CC-Canto's Jyutping into the gloss for pyglossary's EDICT2 reader.

CC-Canto lines add a ``{jyutping}`` block that the stock EDICT2 reader cannot
parse, so it silently drops every entry.  Rewrite each entry from::

    TRAD SIMP [pinyin] {jyutping} /gloss.../

into the plain EDICT2 shape the reader accepts while preserving the Jyutping as
the first gloss sense::

    TRAD SIMP [pinyin] /Jyutping: jyutping/gloss.../
"""

import re
import sys

ENTRY = re.compile(r"^(.*?\])\s*\{([^}]*)\}\s*(/.*)$")


def main() -> None:
    src, dst = sys.argv[1], sys.argv[2]
    kept = 0
    with (
        open(src, encoding="utf-8") as inp,
        open(dst, "w", encoding="utf-8") as out,
    ):
        for line in inp:
            if line.startswith("#") or not line.strip():
                out.write(line)
                continue
            match = ENTRY.match(line.rstrip("\n"))
            if match is None:
                out.write(line)
                continue
            head, jyutping, gloss = (
                match.group(1),
                match.group(2).strip(),
                match.group(3),
            )
            out.write(f"{head} /Jyutping: {jyutping}{gloss}\n")
            kept += 1
    print(f"canto_prep: folded {kept} entries", file=sys.stderr)


if __name__ == "__main__":
    main()
