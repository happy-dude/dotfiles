"""Convert the kengdic Korean-English TSV into a pyglossary Tabfile.

kengdic ships one TSV row per sense with columns ``id, surface, hanja, gloss,
level, created, source``.  Collapse the rows that share a surface form into a
single headword whose definition lists every distinct sense, appending the
Hanja in brackets when present, and emit ``headword<TAB>definition`` lines with
Tabfile escaping so pyglossary's Tabfile reader can build a StarDict.
"""

import csv
import sys


def escape(text: str) -> str:
    return text.replace("\\", "\\\\").replace("\t", "\\t").replace("\n", "\\n")


def main() -> None:
    src, dst = sys.argv[1], sys.argv[2]
    entries: dict[str, list[str]] = {}
    with open(src, encoding="utf-8") as inp:
        for row in csv.DictReader(inp, delimiter="\t"):
            surface = (row.get("surface") or "").strip()
            gloss = (row.get("gloss") or "").strip()
            hanja = (row.get("hanja") or "").strip()
            if not surface or not gloss:
                continue
            sense = f"{gloss} [{hanja}]" if hanja else gloss
            senses = entries.setdefault(surface, [])
            if sense not in senses:
                senses.append(sense)

    with open(dst, "w", encoding="utf-8") as out:
        for surface, senses in entries.items():
            headword = surface.replace("\t", " ").replace("\n", " ")
            out.write(f"{escape(headword)}\t{escape('; '.join(senses))}\n")
    print(f"kengdic_prep: wrote {len(entries)} headwords", file=sys.stderr)


if __name__ == "__main__":
    main()
