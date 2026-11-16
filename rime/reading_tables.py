"""Build the reading tables the Rime annotation filters consult.

The annotation filters show a reading for every character of a candidate.
Rime's reverse dictionaries return a character's readings in code order,
so the first one is the alphabetically smallest, not the commonest: 長 gives
chang2 before zhang3 and 樂 gives lok3 before lok6. This helper reads the
locked dictionaries and writes the Lua module lua/readings.lua loads, which
holds what the reverse dictionaries cannot say:

- ranked: a character's readings by descending dictionary weight, for the
  characters where that order differs from code order. Equal weights are
  ordered by luna_pinyin's weight for the spelling (terra_pinyin gives 呢
  ne5, ni1, and ni2 a third each), then by how many dictionary phrases use
  the reading, then by code.
- phrases: the reading of a phrase, for the phrases where choosing each
  character's top-ranked reading would give a different answer. The filters
  match phrases from the left, longest first, so a phrase that a shorter
  listed phrase already gets right is left out too.

Both hold only entries that change an answer, and phrases below a frequency
floor are left out, so the module stays small enough to load at startup.

A Jyutping phrase reading comes from the Cantonese word list. A Mandarin
one comes from two dictionaries: terra_pinyin has the tones, and
luna_pinyin, which the Pinyin candidates come from, has the spelling people
type. A phrase only one of them lists is read as that one says, with
luna_pinyin's syllables given the top-ranked terra_pinyin tone. A phrase
both list is read as terra_pinyin says when the two spell it alike, and is
taken from neither when they do not: terra_pinyin reads 中的 only as
zhong4 di4 (to hit the mark), luna_pinyin prefers zhong de, and nothing
here can tell which of the two a candidate is.

A phrase the other language lists and this one does not is read by
inference. Readings are paired character by character across the phrases
both languages list: 長 read zoeng2 is zhang3 in nearly every such phrase,
so 長大 (zoeng2 daai6), which no Mandarin dictionary lists, is read
zhang3 da4. A pairing is used only with at least MIN_SUPPORT phrases behind
it and MIN_SHARE of them agreeing.
"""

import argparse
import re
from collections import Counter, defaultdict
from collections.abc import Iterable, Iterator, Mapping
from pathlib import Path
from typing import NamedTuple

from dotfiles_files import fail

Reading = tuple[str, ...]
Ranking = Mapping[str, list[str]]
Pairings = Mapping[tuple[str, str], Counter[str]]

MIN_SUPPORT = 5
MIN_SHARE = 0.95

_TONE = re.compile(r"\d+$")


class Entry(NamedTuple):
    text: str
    code: Reading
    weight: float


class Dictionary(NamedTuple):
    """Single-character readings with weights, and each phrase's readings.

    A phrase's readings are ordered likeliest first.
    """

    chars: dict[str, dict[str, float]]
    phrases: dict[str, list[Reading]]

    def likeliest(self) -> dict[str, Reading]:
        return {text: readings[0] for text, readings in self.phrases.items()}


class Tables(NamedTuple):
    ranked: dict[str, list[str]]
    phrases: dict[str, Reading]


def toneless(code: str) -> str:
    return _TONE.sub("", code)


def _weight(field: str, path: Path) -> float:
    # A reading without a weight is as common as the character itself. The
    # weights are compared only among the readings of one entry.
    if not field:
        return 1.0
    try:
        if field.endswith("%"):
            return float(field[:-1]) / 100
        return float(field)
    except ValueError:
        fail(f"{path}: cannot read the weight {field!r}")


def read_entries(path: Path) -> Iterator[Entry]:
    """Yield the coded rows of a Rime dictionary whose text and code align.

    Rows without a code take their reading from the characters, and rows
    whose code has another number of syllables than the text has characters
    (a letter in the text, a two-syllable character) cannot be aligned.
    """
    in_body = False
    with path.open(encoding="utf-8") as lines:
        for line in lines:
            line = line.rstrip("\n")
            if not in_body:
                if line.startswith("columns:"):
                    fail(f"{path}: a columns: header is not supported")
                in_body = line == "..."
                continue
            if not line or line.startswith("#"):
                continue
            fields = line.split("\t")
            if len(fields) < 2:
                continue
            code = tuple(fields[1].split())
            if len(code) != len(fields[0]):
                continue
            weight = _weight(fields[2] if len(fields) > 2 else "", path)
            yield Entry(fields[0], code, weight)
    if not in_body:
        fail(f"{path}: no '...' line ends the dictionary header")


def load_dictionary(paths: Iterable[Path]) -> Dictionary:
    chars: dict[str, dict[str, float]] = defaultdict(dict)
    listed: dict[str, list[Entry]] = defaultdict(list)
    for path in paths:
        for entry in read_entries(path):
            if len(entry.text) == 1:
                readings = chars[entry.text]
                readings[entry.code[0]] = max(
                    entry.weight, readings.get(entry.code[0], 0.0)
                )
            else:
                listed[entry.text].append(entry)

    def likelihood(entry: Entry) -> tuple[float, float]:
        # Dictionaries list a phrase's alternative readings (什麼 as she2 me5
        # and shen2 me5) mostly without weights; the commoner readings of
        # the characters then decide.
        common = 1.0
        for char, reading in zip(entry.text, entry.code, strict=True):
            common *= chars.get(char, {}).get(reading, 0.0)
        return (entry.weight, common)

    # The sort is stable, so file order settles the rest.
    return Dictionary(
        dict(chars),
        {
            text: [
                entry.code
                for entry in sorted(entries, key=likelihood, reverse=True)
            ]
            for text, entries in listed.items()
        },
    )


def read_frequencies(paths: Iterable[Path]) -> dict[str, int]:
    """Read essay files (text, tab, count), keeping each text's highest."""
    frequencies: dict[str, int] = {}
    for path in paths:
        with path.open(encoding="utf-8") as lines:
            for line in lines:
                fields = line.rstrip("\n").split("\t")
                if len(fields) != 2 or not fields[1].isdigit():
                    continue
                frequencies[fields[0]] = max(
                    int(fields[1]), frequencies.get(fields[0], 0)
                )
    return frequencies


def rank(
    dictionary: Dictionary, plain: Dictionary | None = None
) -> dict[str, list[str]]:
    """Order each character's readings, commonest first.

    By weight, then by the weight `plain`, a toneless dictionary of the same
    language, gives the spelling, then by use in phrases, then by code.
    """
    uses: Counter[tuple[str, str]] = Counter()
    for text, code in dictionary.likeliest().items():
        uses.update(zip(text, code, strict=True))
    spellings = plain.chars if plain else {}
    return {
        char: sorted(
            readings,
            key=lambda reading, char=char, readings=readings: (
                -readings[reading],
                -spellings.get(char, {}).get(toneless(reading), 0.0),
                -uses[char, reading],
                reading,
            ),
        )
        for char, readings in dictionary.chars.items()
    }


def add_tones(text: str, code: Reading, ranking: Ranking) -> Reading | None:
    """Give a toneless reading the top-ranked tone of each syllable."""
    reading = []
    for char, syllable in zip(text, code, strict=True):
        match = next(
            (
                candidate
                for candidate in ranking.get(char, [])
                if toneless(candidate) == syllable
            ),
            None,
        )
        if match is None:
            return None
        reading.append(match)
    return tuple(reading)


def reconcile(
    tonal: Dictionary, plain: Dictionary, ranking: Ranking
) -> dict[str, Reading]:
    """One reading per phrase from a tonal and a toneless dictionary.

    A phrase both list is kept only when the tonal dictionary has the
    toneless one's likeliest spelling; the module docstring says why.
    """
    readings: dict[str, Reading] = {}
    for text, alternatives in tonal.phrases.items():
        spellings = plain.phrases.get(text)
        if spellings is None:
            readings[text] = alternatives[0]
            continue
        for alternative in alternatives:
            if tuple(map(toneless, alternative)) == spellings[0]:
                readings[text] = alternative
                break
    for text, spellings in plain.phrases.items():
        if text not in tonal.phrases:
            toned = add_tones(text, spellings[0], ranking)
            if toned is not None:
                readings[text] = toned
    return readings


def pair(
    source: Mapping[str, Reading], target: Mapping[str, Reading]
) -> dict[tuple[str, str], Counter[str]]:
    """Count, per character and source reading, the target readings."""
    pairings: dict[tuple[str, str], Counter[str]] = defaultdict(Counter)
    for text, code in source.items():
        other = target.get(text)
        if other is None:
            continue
        for char, reading, counterpart in zip(text, code, other, strict=True):
            pairings[char, reading][counterpart] += 1
    return dict(pairings)


def _paired(counts: Counter[str] | None) -> str | None:
    if not counts:
        return None
    reading, support = counts.most_common(1)[0]
    if support < MIN_SUPPORT or support / counts.total() < MIN_SHARE:
        return None
    return reading


def infer(
    source: Mapping[str, Reading],
    ranking: Ranking,
    pairings: Pairings,
    known: Mapping[str, Reading],
) -> dict[str, Reading]:
    """Read the source language's phrases in the target language.

    A character keeps its top-ranked target reading unless its source
    reading is paired with another one. Phrases the target language already
    lists, phrases no pairing touches, and phrases with a character the
    target language cannot read are left out.
    """
    inferred: dict[str, Reading] = {}
    for text, code in source.items():
        if text in known:
            continue
        reading = []
        informed = False
        for char, source_reading in zip(text, code, strict=True):
            candidates = ranking.get(char)
            if not candidates:
                break
            choice = None
            if len(candidates) > 1:
                choice = _paired(pairings.get((char, source_reading)))
                informed = informed or choice is not None
            reading.append(choice or candidates[0])
        else:
            if informed:
                inferred[text] = tuple(reading)
    return inferred


def resolve(
    text: str,
    ranking: Ranking,
    phrases: Mapping[str, Reading],
    max_length: int,
) -> list[str | None]:
    """What lua/readings.lua answers for a candidate nobody typed.

    Keep this in step with resolve() there: build_tables() leaves out every
    phrase this already gets right.
    """
    reading: list[str | None] = []
    position = 0
    while position < len(text):
        longest = min(max_length, len(text) - position)
        for length in range(longest, 1, -1):
            end = position + length
            phrase = phrases.get(text[position:end])
            if phrase is not None:
                reading.extend(phrase)
                position = end
                break
        else:
            candidates = ranking.get(text[position])
            reading.append(candidates[0] if candidates else None)
            position += 1
    return reading


def build_tables(
    ranking: Ranking,
    phrases: Mapping[str, Reading],
    max_length: int,
) -> Tables:
    """Keep only the rankings and phrases that change an answer."""
    kept: dict[str, Reading] = {}
    # Shorter phrases first: a longer one is tested against them.
    for text in sorted(phrases, key=lambda text: (len(text), text)):
        if len(text) > max_length:
            continue
        if resolve(text, ranking, kept, max_length) != list(phrases[text]):
            kept[text] = phrases[text]
    return Tables(
        {
            char: readings
            for char, readings in ranking.items()
            if readings != sorted(readings)
        },
        kept,
    )


def _frequent(
    phrases: Mapping[str, Reading],
    frequencies: Mapping[str, int] | None,
    floor: int,
) -> dict[str, Reading]:
    if frequencies is None:
        return dict(phrases)
    return {
        text: code
        for text, code in phrases.items()
        if frequencies.get(text, 0) >= floor
    }


def generate(
    pinyin: Dictionary,
    toneless_pinyin: Dictionary,
    jyutping: Dictionary,
    frequencies: Mapping[str, int] | None,
    min_frequency: int,
    max_length: int,
) -> dict[str, Tables]:
    pinyin_ranking = rank(pinyin, toneless_pinyin)
    jyutping_ranking = rank(jyutping)
    mandarin = reconcile(pinyin, toneless_pinyin, pinyin_ranking)
    cantonese = jyutping.likeliest()

    mandarin_inferred = infer(
        cantonese, pinyin_ranking, pair(cantonese, mandarin), mandarin
    )
    cantonese_inferred = infer(
        mandarin, jyutping_ranking, pair(mandarin, cantonese), cantonese
    )
    return {
        "pinyin": build_tables(
            pinyin_ranking,
            _frequent(
                mandarin_inferred | mandarin, frequencies, min_frequency
            ),
            max_length,
        ),
        "jyutping": build_tables(
            jyutping_ranking,
            _frequent(
                cantonese_inferred | cantonese, frequencies, min_frequency
            ),
            max_length,
        ),
    }


def _lua_string(text: str) -> str:
    return "'" + text.replace("\\", "\\\\").replace("'", "\\'") + "'"


def render(tables: Mapping[str, Tables], sources: Iterable[str]) -> str:
    lines = [
        "-- Generated by rime/reading_tables.py from the dictionaries below;",
        "-- do not edit. lua/readings.lua says how the tables are used.",
        *(f"--   {source}" for source in sources),
        "return {",
    ]
    # No indentation: the module is parsed on every start and never read.
    for language, table in tables.items():
        longest = max((len(text) for text in table.phrases), default=1)
        lines.append(f"{language} = {{")
        lines.append(f"max_phrase_length = {longest},")
        lines.append("ranked = {")
        lines.extend(
            f"[{_lua_string(char)}]={_lua_string(' '.join(readings))},"
            for char, readings in sorted(table.ranked.items())
        )
        lines.append("},")
        lines.append("phrases = {")
        lines.extend(
            f"[{_lua_string(text)}]={_lua_string(' '.join(code))},"
            for text, code in sorted(table.phrases.items())
        )
        lines.append("},")
        lines.append("},")
    lines.append("}")
    return "\n".join(lines) + "\n"


def _describe(path: Path) -> str:
    """Name a dictionary by the name and version in its header."""
    found: dict[str, str] = {}
    with path.open(encoding="utf-8") as lines:
        for line in lines:
            if line.rstrip("\n") == "...":
                break
            key, _, value = line.partition(":")
            if key in ("name", "version"):
                found[key] = value.strip().strip("\"'")
    return f"{found.get('name', path.name)} {found.get('version', '')}".strip()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--pinyin",
        type=Path,
        action="append",
        required=True,
        help="Mandarin dictionary with tones (terra_pinyin)",
    )
    parser.add_argument(
        "--toneless-pinyin",
        type=Path,
        action="append",
        default=[],
        help="Mandarin dictionary without tones (luna_pinyin)",
    )
    parser.add_argument(
        "--jyutping",
        type=Path,
        action="append",
        required=True,
        help="Cantonese dictionary (jyut6ping3 characters and words)",
    )
    parser.add_argument(
        "--frequency",
        type=Path,
        action="append",
        default=[],
        help="essay file; phrases below --min-frequency in all are dropped",
    )
    parser.add_argument("--min-frequency", type=int, default=0)
    parser.add_argument("--max-length", type=int, default=8)
    parser.add_argument("--output", type=Path, required=True)
    arguments = parser.parse_args()

    tables = generate(
        load_dictionary(arguments.pinyin),
        load_dictionary(arguments.toneless_pinyin),
        load_dictionary(arguments.jyutping),
        read_frequencies(arguments.frequency) if arguments.frequency else None,
        arguments.min_frequency,
        arguments.max_length,
    )
    for language, table in tables.items():
        if not table.ranked:
            fail(f"no {language} rankings: is the dictionary the right one?")
    dictionaries = [
        *arguments.pinyin,
        *arguments.toneless_pinyin,
        *arguments.jyutping,
    ]
    arguments.output.write_text(
        render(tables, map(_describe, dictionaries)), encoding="utf-8"
    )


if __name__ == "__main__":
    main()
