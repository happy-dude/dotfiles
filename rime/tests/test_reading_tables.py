import contextlib
import io
import tempfile
import unittest
from collections import Counter
from pathlib import Path

import reading_tables
from reading_tables import Dictionary, Tables


class ReadingTablesTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)

    def dictionary(self, name: str, *rows: str) -> Path:
        path = self.root / name
        path.write_text(
            "\n".join(["# comment", "---", f"name: {name}", "...", "", *rows])
            + "\n",
            encoding="utf-8",
        )
        return path

    def load(self, name: str, *rows: str) -> Dictionary:
        return reading_tables.load_dictionary([self.dictionary(name, *rows)])

    def assert_fails(self, message: str, path: Path) -> None:
        stderr = io.StringIO()
        with (
            contextlib.redirect_stderr(stderr),
            self.assertRaises(SystemExit),
        ):
            list(reading_tables.read_entries(path))
        self.assertIn(message, stderr.getvalue())

    def test_rows_that_cannot_be_aligned_are_skipped(self):
        path = self.dictionary(
            "d",
            "長\tchang2\t90%",
            "# a comment",
            "長大",
            "卡拉OK\tka3 la1",
            "兛\tcin1 hak1",
            "大\tda4",
            "重\tzhong4\t3",
        )
        self.assertEqual(
            list(reading_tables.read_entries(path)),
            [
                ("長", ("chang2",), 0.9),
                ("大", ("da4",), 1.0),
                ("重", ("zhong4",), 3.0),
            ],
        )

    def test_a_format_this_reader_does_not_know_fails(self):
        path = self.root / "columns"
        path.write_text("---\ncolumns:\n  - code\n...\nchang2\t長\n")
        self.assert_fails("columns: header is not supported", path)
        path = self.root / "headless"
        path.write_text("長\tchang2\n")
        self.assert_fails("no '...' line ends the dictionary header", path)
        path = self.dictionary("weight", "長\tchang2\tmost")
        self.assert_fails("cannot read the weight 'most'", path)

    def test_readings_rank_by_weight_then_use_then_code(self):
        dictionary = self.load(
            "d",
            "長\tchang2\t90%",
            "長\tzhang3\t10%",
            "重\tchong2\t10%",
            "重\tzhong4\t90%",
            # Equal weights: 亞洲 uses ya4, so it ranks first.
            "亞\tya3",
            "亞\tya4",
            "亞洲\tya4 zhou1",
            # Equal weights and no phrase: code order.
            "誰\tshui2",
            "誰\tshei2",
            "呢\tne5\t33.33%",
            "呢\tni1\t33.33%",
            "呢\tni2\t33.33%",
            "呢喃\tni2 nan2",
        )
        ranking = {
            "長": ["chang2", "zhang3"],
            "重": ["zhong4", "chong2"],
            "亞": ["ya4", "ya3"],
            "誰": ["shei2", "shui2"],
            "呢": ["ni2", "ne5", "ni1"],
        }
        self.assertEqual(reading_tables.rank(dictionary), ranking)
        # A toneless dictionary's weights come before the phrases, and
        # never before the tonal dictionary's own weights.
        plain = self.load(
            "plain",
            "呢\tne\t98.97%",
            "呢\tni\t1.03%",
            "長\tchang\t1%",
            "長\tzhang\t99%",
        )
        self.assertEqual(
            reading_tables.rank(dictionary, plain),
            ranking | {"呢": ["ne5", "ni2", "ni1"]},
        )

    def test_a_phrase_with_several_readings_lists_the_likeliest_first(self):
        dictionary = self.load(
            "d",
            "什\tshe2\t0%",
            "什\tshen2\t1%",
            "什\tshi2\t99%",
            "麼\tme5",
            # No weights: the commoner reading of 什 decides.
            "什麼\tshe2 me5",
            "什麼\tshen2 me5",
            # A weight on the phrase outranks the characters.
            "什麼的\tshen2 me5 de5\t10%",
            "什麼的\tshe2 me5 de5\t90%",
        )
        self.assertEqual(
            dictionary.phrases,
            {
                "什麼": [("shen2", "me5"), ("she2", "me5")],
                "什麼的": [("she2", "me5", "de5"), ("shen2", "me5", "de5")],
            },
        )

    def test_the_two_mandarin_dictionaries_are_reconciled(self):
        tonal = self.load(
            "terra",
            "長\tchang2\t90%",
            "長\tzhang3\t10%",
            "中\tzhong1\t90%",
            "中\tzhong4\t10%",
            "的\tde5\t98%",
            "的\tdi4\t1%",
            "大\tda4",
            "校\txiao4",
            "什\tshen2\t1%",
            "什\tshi2\t99%",
            "麼\tme5",
            "校長\txiao4 zhang3",
            "中的\tzhong4 di4",
            "什麼\tshi2 me5",
            "什麼\tshen2 me5",
        )
        plain = self.load(
            "luna",
            "長\tchang\t60%",
            "長\tzhang\t40%",
            "中\tzhong",
            "的\tde\t99%",
            "的\tdi\t1%",
            "什\tshen\t84%",
            "什\tshi\t16%",
            "麼\tme",
            "長大\tzhang da",
            "中的\tzhong de",
            "中的\tzhong di",
            "什麼\tshen me",
            "長X\tzhang x",
        )
        self.assertEqual(
            reading_tables.reconcile(tonal, plain, reading_tables.rank(tonal)),
            {
                # Only the tonal dictionary lists it.
                "校長": ("xiao4", "zhang3"),
                # Both list it: the tonal reading with the spelling the
                # toneless dictionary prefers, though not the tonal
                # dictionary's own likeliest.
                "什麼": ("shen2", "me5"),
                # Only the toneless dictionary lists it: tones by rank.
                "長大": ("zhang3", "da4"),
                # 中的 is spelled differently by the two and is left out, and
                # 長X has a syllable no tonal reading spells.
            },
        )

    def test_readings_are_inferred_from_a_consistent_pairing(self):
        ranking = {"長": ["chang2", "zhang3"], "大": ["da4"], "城": ["cheng2"]}
        pairings = {
            ("長", "zoeng2"): Counter(zhang3=reading_tables.MIN_SUPPORT),
            # Too few phrases.
            ("長", "zoeng6"): Counter(zhang3=reading_tables.MIN_SUPPORT - 1),
            # Too little agreement.
            ("長", "coeng4"): Counter(chang2=80, zhang3=20),
        }
        source = {
            "長大": ("zoeng2", "daai6"),
            "大長": ("daai6", "zoeng6"),
            "長城": ("coeng4", "sing4"),
            "長長": ("zoeng2", "coeng4"),
            "校長": ("haau6", "zoeng2"),
            "已知": ("ji5", "zi1"),
        }
        self.assertEqual(
            reading_tables.infer(
                source, ranking, pairings, {"已知": ("yi3", "zhi1")}
            ),
            {
                "長大": ("zhang3", "da4"),
                # The unpaired character keeps its top-ranked reading.
                "長長": ("zhang3", "chang2"),
                # 大長 and 長城 have no usable pairing, 校 has no reading in
                # the target language, and 已知 is already listed.
            },
        )

    def test_pairings_count_readings_character_by_character(self):
        self.assertEqual(
            reading_tables.pair(
                {
                    "校長": ("haau6", "zoeng2"),
                    "長大": ("zoeng2", "daai6"),
                    "長城": ("coeng4", "sing4"),
                },
                {"校長": ("xiao4", "zhang3"), "長大": ("zhang3", "da4")},
            ),
            {
                ("校", "haau6"): Counter(xiao4=1),
                ("長", "zoeng2"): Counter(zhang3=2),
                ("大", "daai6"): Counter(da4=1),
            },
        )

    def test_only_what_changes_an_answer_is_kept(self):
        ranking = {
            "長": ["chang2", "zhang3"],
            "重": ["zhong4", "chong2"],
            "大": ["da4", "dai4"],
            "後": ["hou4"],
            "城": ["cheng2"],
        }
        tables = reading_tables.build_tables(
            ranking,
            {
                "長大": ("zhang3", "da4"),
                # The top-ranked readings already say this.
                "長城": ("chang2", "cheng2"),
                # 長大 is listed, and is matched first.
                "長大後": ("zhang3", "da4", "hou4"),
                # ... but not here, where it would be wrong.
                "長大城": ("chang2", "da4", "cheng2"),
                # Longer than the filters look for.
                "大大大長": ("da4", "da4", "da4", "zhang3"),
            },
            3,
        )
        self.assertEqual(
            tables,
            Tables(
                # 長 and 大 rank in code order, which the filters get from
                # the reverse dictionary.
                {"重": ["zhong4", "chong2"]},
                {
                    "長大": ("zhang3", "da4"),
                    "長大城": ("chang2", "da4", "cheng2"),
                },
            ),
        )
        # The answers the filters give with those tables.
        for text, expected in {
            "長大後": ["zhang3", "da4", "hou4"],
            "長大城": ["chang2", "da4", "cheng2"],
            "重長大": ["zhong4", "zhang3", "da4"],
            "大長大城": ["da4", "chang2", "da4", "cheng2"],
            "長?": ["chang2", None],
        }.items():
            self.assertEqual(
                reading_tables.resolve(text, ranking, tables.phrases, 3),
                expected,
                text,
            )

    def test_rare_phrases_are_left_out(self):
        pinyin = self.load(
            "terra",
            "長\tchang2\t90%",
            "長\tzhang3\t10%",
            "大\tda4",
            "老\tlao3",
            "長大\tzhang3 da4",
            "長老\tzhang3 lao3",
        )
        empty = Dictionary({}, {})
        jyutping = self.load("jyut", "長\tcoeng4", "長\tzoeng2\t5%")

        def phrases(frequencies, floor):
            tables = reading_tables.generate(
                pinyin, empty, jyutping, frequencies, floor, 8
            )
            return sorted(tables["pinyin"].phrases)

        self.assertEqual(phrases(None, 500), ["長大", "長老"])
        self.assertEqual(phrases({"長大": 500, "長老": 499}, 500), ["長大"])
        self.assertEqual(phrases({}, 1), [])

    def test_frequencies_keep_the_highest_count(self):
        first = self.root / "essay"
        first.write_text("長大\t5055\n長老\t12\nnot a row\n", encoding="utf-8")
        second = self.root / "essay-cantonese"
        second.write_text("長大\t7830\n長老\t3\n", encoding="utf-8")
        self.assertEqual(
            reading_tables.read_frequencies([first, second]),
            {"長大": 7830, "長老": 12},
        )

    def test_the_module_is_valid_lua_source(self):
        tables = {
            "pinyin": Tables(
                {"重": ["zhong4", "chong2"]},
                {
                    "長大": ("zhang3", "da4"),
                    "it's\\": ("a", "b", "c", "d", "e"),
                },
            ),
            "jyutping": Tables({}, {}),
        }
        self.assertEqual(
            reading_tables.render(tables, ["terra_pinyin 1", "luna 2"]),
            "-- Generated by rime/reading_tables.py from the dictionaries"
            " below;\n"
            "-- do not edit. lua/readings.lua says how the tables are used.\n"
            "--   terra_pinyin 1\n"
            "--   luna 2\n"
            "return {\n"
            "pinyin = {\n"
            "max_phrase_length = 5,\n"
            "ranked = {\n"
            "['重']='zhong4 chong2',\n"
            "},\n"
            "phrases = {\n"
            "['it\\'s\\\\']='a b c d e',\n"
            "['長大']='zhang3 da4',\n"
            "},\n"
            "},\n"
            "jyutping = {\n"
            "max_phrase_length = 1,\n"
            "ranked = {\n"
            "},\n"
            "phrases = {\n"
            "},\n"
            "},\n"
            "}\n",
        )


if __name__ == "__main__":
    unittest.main()
