"""Known-answer tests for checks/rainbow_palette.py."""

from rainbow_palette import (
    apca_contrast,
    ciede2000,
    parse_hex,
    score,
    terminal_index_failures,
)

# Sharma, Wu and Dalal (2005), table 1: L*a*b* pairs and their CIEDE2000
# difference, rounded to four places. Pairs 7-16 exercise the hue-angle
# wrap and the mean-hue branches.
# ref: https://hajim.rochester.edu/ece/sites/gsharma/ciede2000/
SHARMA_PAIRS = (
    ((50.0000, 2.6772, -79.7751), (50.0000, 0.0000, -82.7485), 2.0425),
    ((50.0000, 3.1571, -77.2803), (50.0000, 0.0000, -82.7485), 2.8615),
    ((50.0000, 2.8361, -74.0200), (50.0000, 0.0000, -82.7485), 3.4412),
    ((50.0000, -1.3802, -84.2814), (50.0000, 0.0000, -82.7485), 1.0000),
    ((50.0000, -1.1848, -84.8006), (50.0000, 0.0000, -82.7485), 1.0000),
    ((50.0000, -0.9009, -85.5211), (50.0000, 0.0000, -82.7485), 1.0000),
    ((50.0000, 0.0000, 0.0000), (50.0000, -1.0000, 2.0000), 2.3669),
    ((50.0000, -1.0000, 2.0000), (50.0000, 0.0000, 0.0000), 2.3669),
    ((50.0000, 2.4900, -0.0010), (50.0000, -2.4900, 0.0009), 7.1792),
    ((50.0000, 2.4900, -0.0010), (50.0000, -2.4900, 0.0010), 7.1792),
    ((50.0000, 2.4900, -0.0010), (50.0000, -2.4900, 0.0011), 7.2195),
    ((50.0000, 2.4900, -0.0010), (50.0000, -2.4900, 0.0012), 7.2195),
    ((50.0000, -0.0010, 2.4900), (50.0000, 0.0009, -2.4900), 4.8045),
    ((50.0000, -0.0010, 2.4900), (50.0000, 0.0010, -2.4900), 4.8045),
    ((50.0000, -0.0010, 2.4900), (50.0000, 0.0011, -2.4900), 4.7461),
    ((50.0000, 2.5000, 0.0000), (50.0000, 0.0000, -2.5000), 4.3065),
    ((50.0000, 2.5000, 0.0000), (73.0000, 25.0000, -18.0000), 27.1492),
    ((50.0000, 2.5000, 0.0000), (61.0000, -5.0000, 29.0000), 22.8977),
    ((50.0000, 2.5000, 0.0000), (56.0000, -27.0000, -3.0000), 31.9030),
    ((50.0000, 2.5000, 0.0000), (58.0000, 24.0000, 15.0000), 19.4535),
    ((50.0000, 2.5000, 0.0000), (50.0000, 3.1736, 0.5854), 1.0000),
    ((50.0000, 2.5000, 0.0000), (50.0000, 3.2972, 0.0000), 1.0000),
    ((50.0000, 2.5000, 0.0000), (50.0000, 1.8634, 0.5757), 1.0000),
    ((50.0000, 2.5000, 0.0000), (50.0000, 3.2592, 0.3350), 1.0000),
    ((60.2574, -34.0099, 36.2677), (60.4626, -34.1751, 39.4387), 1.2644),
    ((63.0109, -31.0961, -5.8663), (62.8187, -29.7946, -4.0864), 1.2630),
    ((61.2901, 3.7196, -5.3901), (61.4292, 2.2480, -4.9620), 1.8731),
    ((35.0831, -44.1164, 3.7933), (35.0232, -40.0716, 1.5901), 1.8645),
    ((22.7233, 20.0904, -46.6940), (23.0331, 14.9730, -42.5619), 2.0373),
    ((36.4612, 47.8580, 18.3852), (36.2715, 50.5065, 21.2231), 1.4146),
    ((90.8027, -2.0831, 1.4410), (91.1528, -1.6435, 0.0447), 1.4441),
    ((90.9257, -0.5406, -0.9208), (88.6381, -0.8985, -0.7239), 1.5381),
    ((6.7747, -0.2908, -2.4247), (5.8714, -0.0985, -2.2286), 0.6377),
    ((2.0776, 0.0795, -1.1350), (0.9033, -0.0636, -0.5514), 0.9082),
)

# The APCA W3 keystone values, text then background, unrounded. The
# published values are signed; apca_contrast returns the magnitude.
# ref: https://github.com/Myndex/SAPC-APCA/tree/master/documentation
APCA_PAIRS = (
    ("#888888", "#ffffff", 63.056469930209424),
    ("#ffffff", "#888888", 68.54146436644962),
    ("#000000", "#aaaaaa", 58.146262578561334),
    ("#aaaaaa", "#000000", 56.24113336839742),
    ("#112233", "#ddeeff", 91.66830811481631),
    ("#ddeeff", "#112233", 93.06770049484275),
    ("#112233", "#444444", 8.32326136957393),
    ("#444444", "#112233", 7.526878460278154),
)

# The ten-colour list Vim used before lib/rainbow-delimiters.nix, scored on
# the mix dark-medium background and text.
OLD_VIM_PALETTE = [
    "#FE2712",
    "#66B032",
    "#0392CE",
    "#FD5308",
    "#0247FE",
    "#FB9902",
    "#8601AF",
    "#FEFE33",
    "#A7194B",
    "#D0EA2B",
]
BACKGROUND = "#282828"
TEXT = "#e2cca9"


def categories(failures: list[str]) -> set[str]:
    return {failure.split(":", 1)[0] for failure in failures}


def main() -> None:
    for number, (first, second, expected) in enumerate(SHARMA_PAIRS, 1):
        for left, right in ((first, second), (second, first)):
            actual = ciede2000(left, right)
            assert abs(actual - expected) < 5e-5, (number, actual, expected)

    for text, background, expected in APCA_PAIRS:
        actual = apca_contrast(parse_hex(text), parse_hex(background))
        assert abs(actual - expected) < 1e-9, (text, background, actual)

    old = score(OLD_VIM_PALETTE, BACKGROUND, TEXT, [])
    assert categories(old) == {
        "depths",
        "two apart",
        "colour-blind next",
        "colour-blind two apart",
        "distinct",
        "lightness",
        "contrast",
    }, old
    assert (
        "two apart: depths 8 and 10 differ by 7.6 under normal vision; "
        "the floor is 25.0"
    ) in old, old

    # Only the wrap from the last depth back to the first is too close.
    wrapped = score(
        ["#e9b143", "#60a7dd", "#ddb7fc", "#e9b144"], BACKGROUND, TEXT, []
    )
    assert any(f.startswith("next: depths 4 and 1 ") for f in wrapped), wrapped

    assert score(["#e9b143", "#60a7dd"], BACKGROUND, TEXT, []) == [
        "needs at least 3 colours, has 2"
    ]
    assert terminal_index_failures([0, 15]) == []
    assert terminal_index_failures([16, -1, "11", True, 3.0]) == [
        "cterm: depth 1 has 16; it must be an integer 0-15",
        "cterm: depth 2 has -1; it must be an integer 0-15",
        "cterm: depth 3 has '11'; it must be an integer 0-15",
        "cterm: depth 4 has True; it must be an integer 0-15",
        "cterm: depth 5 has 3.0; it must be an integer 0-15",
    ]


if __name__ == "__main__":
    main()
