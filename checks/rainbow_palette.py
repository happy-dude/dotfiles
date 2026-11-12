"""Score a rainbow-delimiter palette and fail when it falls short.

Usage: rainbow_palette.py PALETTE.json BACKGROUND TEXT [AVOID...]

PALETTE.json is lib/rainbow-delimiters.nix as JSON: one object per nesting
depth, in depth order, with a ``gui`` hex colour and a ``cterm`` index
into the terminal's 16-colour palette. BACKGROUND and TEXT are the
editor's background and normal-text colours. Each AVOID colour commonly
sits right beside a delimiter, such as a function name after ``(``, and
every delimiter colour must stay clear of it.

Editors cycle the list, so the last depth is followed by the first. Each
pairwise check takes the weakest pair around that cycle:

- next: depth d against depth d+1.
- two apart: depth d against d+2, as in ``f(g(h(x)))``.
- colour-blind: both of those after simulating protanopia, deuteranopia
  and tritanopia (Machado, Oliveira and Fernandes 2009, severity 1.0).

Differences are CIEDE2000 and contrast is APCA (W3 0.0.98G-4g). The floors
are design choices for this palette, not accessibility standards. Only the
``gui`` colours are scored; ``cterm`` is checked for range alone.
"""

import json
import math
import re
import sys
from itertools import combinations
from pathlib import Path

# Emacs's rainbow-delimiters defines rainbow-delimiters-depth-1-face to
# depth-9 only; a tenth colour would have no face to go on.
MAX_DEPTHS = 9

# CIEDE2000 floors; see the module docstring for each pairing.
MIN_NEXT = 15.0
MIN_TWO_APART = 25.0
MIN_COLOUR_BLIND = 10.0
MIN_DISTINCT = 10.0
MIN_FROM_AVOIDED = 12.0
# Neighbouring depths must also differ in OKLab lightness, the cue thin
# glyphs keep best.
MIN_LIGHTNESS_STEP = 0.08
# APCA Lc 45 is the minimum for fine details. The ceiling is the normal
# text's own contrast, so delimiters never outshine the code.
MIN_CONTRAST = 45.0

# Machado et al. (2009), table 1, severity 1.0, applied to linear sRGB.
# ref: https://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html
DEFICIENCIES = {
    "protanopia": (
        (0.152286, 1.052583, -0.204868),
        (0.114503, 0.786281, 0.099216),
        (-0.003882, -0.048116, 1.051998),
    ),
    "deuteranopia": (
        (0.367322, 0.860646, -0.227968),
        (0.280085, 0.672501, 0.047413),
        (-0.011820, 0.042940, 0.968881),
    ),
    "tritanopia": (
        (1.255528, -0.076749, -0.178779),
        (-0.078411, 0.930809, 0.147602),
        (0.004733, 0.691367, 0.303900),
    ),
}

RGB = tuple[float, float, float]
Lab = tuple[float, float, float]
Matrix = tuple[RGB, RGB, RGB]


def parse_hex(colour: str) -> RGB:
    """Return sRGB channels in [0, 1] for a ``#rrggbb`` string."""
    if not re.fullmatch(r"#[0-9A-Fa-f]{6}", colour):
        raise ValueError(f"not a #rrggbb colour: {colour!r}")
    red, green, blue = (int(colour[i : i + 2], 16) / 255 for i in (1, 3, 5))
    return red, green, blue


def to_linear(value: float) -> float:
    if value <= 0.04045:
        return value / 12.92
    return ((value + 0.055) / 1.055) ** 2.4


def from_linear(value: float) -> float:
    if value <= 0.0031308:
        return 12.92 * value
    return 1.055 * value ** (1 / 2.4) - 0.055


def simulate(rgb: RGB, matrix: Matrix) -> RGB:
    """Apply a colour-vision-deficiency matrix in linear sRGB."""
    linear = [to_linear(channel) for channel in rgb]
    mixed = (
        sum(weight * value for weight, value in zip(row, linear, strict=True))
        for row in matrix
    )
    red, green, blue = (from_linear(min(1.0, max(0.0, v))) for v in mixed)
    return red, green, blue


def oklab_lightness(rgb: RGB) -> float:
    r, g, b = (to_linear(channel) for channel in rgb)
    long = math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
    medium = math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
    short = math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)
    return 0.2104542553 * long + 0.7936177850 * medium - 0.0040720468 * short


def cielab(rgb: RGB) -> Lab:
    """CIE L*a*b* under D65."""
    r, g, b = (to_linear(channel) for channel in rgb)
    x = (0.4124564 * r + 0.3575761 * g + 0.1804375 * b) / 0.95047
    y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
    z = (0.0193339 * r + 0.1191920 * g + 0.9503041 * b) / 1.08883

    def f(t: float) -> float:
        if t > (6 / 29) ** 3:
            return t ** (1 / 3)
        return t / (3 * (6 / 29) ** 2) + 4 / 29

    fx, fy, fz = f(x), f(y), f(z)
    return 116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)


def ciede2000(first: Lab, second: Lab) -> float:
    """CIEDE2000 colour difference (Sharma, Wu and Dalal 2005)."""
    l1, a1, b1 = first
    l2, a2, b2 = second
    c_mean = (math.hypot(a1, b1) + math.hypot(a2, b2)) / 2
    g = 0.5 * (1 - math.sqrt(c_mean**7 / (c_mean**7 + 25**7)))
    a1p, a2p = (1 + g) * a1, (1 + g) * a2
    c1p, c2p = math.hypot(a1p, b1), math.hypot(a2p, b2)
    h1p = math.degrees(math.atan2(b1, a1p)) % 360
    h2p = math.degrees(math.atan2(b2, a2p)) % 360

    if c1p * c2p == 0:
        hue_step = 0.0
    elif abs(h2p - h1p) <= 180:
        hue_step = h2p - h1p
    elif h2p - h1p > 180:
        hue_step = h2p - h1p - 360
    else:
        hue_step = h2p - h1p + 360
    delta_l = l2 - l1
    delta_c = c2p - c1p
    delta_h = 2 * math.sqrt(c1p * c2p) * math.sin(math.radians(hue_step / 2))

    l_mean = (l1 + l2) / 2
    cp_mean = (c1p + c2p) / 2
    if c1p * c2p == 0:
        hp_mean = h1p + h2p
    elif abs(h1p - h2p) <= 180:
        hp_mean = (h1p + h2p) / 2
    elif h1p + h2p < 360:
        hp_mean = (h1p + h2p + 360) / 2
    else:
        hp_mean = (h1p + h2p - 360) / 2

    t = (
        1
        - 0.17 * math.cos(math.radians(hp_mean - 30))
        + 0.24 * math.cos(math.radians(2 * hp_mean))
        + 0.32 * math.cos(math.radians(3 * hp_mean + 6))
        - 0.20 * math.cos(math.radians(4 * hp_mean - 63))
    )
    delta_theta = 30 * math.exp(-(((hp_mean - 275) / 25) ** 2))
    r_c = 2 * math.sqrt(cp_mean**7 / (cp_mean**7 + 25**7))
    s_l = 1 + 0.015 * (l_mean - 50) ** 2 / math.sqrt(20 + (l_mean - 50) ** 2)
    s_c = 1 + 0.045 * cp_mean
    s_h = 1 + 0.015 * cp_mean * t
    r_t = -math.sin(math.radians(2 * delta_theta)) * r_c
    return math.sqrt(
        (delta_l / s_l) ** 2
        + (delta_c / s_c) ** 2
        + (delta_h / s_h) ** 2
        + r_t * (delta_c / s_c) * (delta_h / s_h)
    )


def difference(first: RGB, second: RGB) -> float:
    """CIEDE2000 between two sRGB colours."""
    return ciede2000(cielab(first), cielab(second))


def apca_contrast(text: RGB, background: RGB) -> float:
    """APCA lightness contrast as a magnitude (W3 0.0.98G-4g)."""

    def screen_luminance(rgb: RGB) -> float:
        r, g, b = (channel**2.4 for channel in rgb)
        y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
        return y + (0.022 - y) ** 1.414 if y < 0.022 else y

    y_text = screen_luminance(text)
    y_background = screen_luminance(background)
    if abs(y_background - y_text) < 0.0005:
        return 0.0
    if y_background > y_text:
        contrast = (y_background**0.56 - y_text**0.57) * 1.14
        return 0.0 if contrast < 0.1 else (contrast - 0.027) * 100
    contrast = (y_background**0.65 - y_text**0.62) * 1.14
    return 0.0 if contrast > -0.1 else -(contrast + 0.027) * 100


def weakest_pair(
    views: dict[str, list[RGB]], step: int
) -> tuple[float, str, int]:
    """Smallest difference between depths ``step`` apart, around the cycle.

    Returns the difference, the view it occurred in, and the zero-based
    index of the first depth in that pair.
    """
    return min(
        (difference(seen[i], seen[(i + step) % len(seen)]), name, i)
        for name, seen in views.items()
        for i in range(len(seen))
    )


def score(
    palette: list[str], background: str, text: str, avoided: list[str]
) -> list[str]:
    """Return one message per failed floor; an empty list passes."""
    if len(palette) < 3:
        return [f"needs at least 3 colours, has {len(palette)}"]
    count = len(palette)
    colours = [parse_hex(colour) for colour in palette]
    failures: list[str] = []
    if count > MAX_DEPTHS:
        failures.append(
            f"depths: {count} colours, but Emacs has faces for only "
            f"{MAX_DEPTHS} depths"
        )

    normal = {"normal vision": colours}
    deficient = {
        name: [simulate(colour, matrix) for colour in colours]
        for name, matrix in DEFICIENCIES.items()
    }
    for label, step, views, floor in (
        ("next", 1, normal, MIN_NEXT),
        ("two apart", 2, normal, MIN_TWO_APART),
        ("colour-blind next", 1, deficient, MIN_COLOUR_BLIND),
        ("colour-blind two apart", 2, deficient, MIN_COLOUR_BLIND),
    ):
        weakest, name, first = weakest_pair(views, step)
        if weakest < floor:
            failures.append(
                f"{label}: depths {first + 1} and "
                f"{(first + step) % count + 1} differ by {weakest:.1f} "
                f"under {name}; the floor is {floor}"
            )

    for first, second in combinations(range(count), 2):
        pair_difference = difference(colours[first], colours[second])
        if pair_difference < MIN_DISTINCT:
            failures.append(
                f"distinct: depths {first + 1} and {second + 1} differ by "
                f"{pair_difference:.1f}; the floor is {MIN_DISTINCT}"
            )

    lightness = [oklab_lightness(colour) for colour in colours]
    for i in range(count):
        step = abs(lightness[i] - lightness[(i + 1) % count])
        if step < MIN_LIGHTNESS_STEP:
            failures.append(
                f"lightness: depths {i + 1} and {(i + 1) % count + 1} "
                f"differ by {step:.3f}; the floor is {MIN_LIGHTNESS_STEP}"
            )

    background_rgb = parse_hex(background)
    ceiling = apca_contrast(parse_hex(text), background_rgb)
    for depth, colour in enumerate(colours, start=1):
        contrast = apca_contrast(colour, background_rgb)
        if not MIN_CONTRAST <= contrast <= ceiling:
            failures.append(
                f"contrast: depth {depth} has APCA Lc {contrast:.1f}; it "
                f"must lie between {MIN_CONTRAST} and the text's "
                f"{ceiling:.1f}"
            )
        for avoid in avoided:
            avoid_difference = difference(colour, parse_hex(avoid))
            if avoid_difference < MIN_FROM_AVOIDED:
                failures.append(
                    f"avoid: depth {depth} is {avoid_difference:.1f} from "
                    f"{avoid}; the floor is {MIN_FROM_AVOIDED}"
                )
    return failures


def terminal_index_failures(indices: list[object]) -> list[str]:
    """Return one message per ``cterm`` value outside the 16 colours."""
    return [
        f"cterm: depth {depth} has {index!r}; it must be an integer 0-15"
        for depth, index in enumerate(indices, start=1)
        if type(index) is not int or not 0 <= index <= 15
    ]


def main(argv: list[str]) -> int:
    if len(argv) < 4:
        print(__doc__.splitlines()[2], file=sys.stderr)
        return 2
    entries = json.loads(Path(argv[1]).read_text())
    palette = [entry["gui"] for entry in entries]
    failures = score(palette, argv[2], argv[3], argv[4:])
    failures += terminal_index_failures([entry["cterm"] for entry in entries])
    for failure in failures:
        print(f"rainbow palette: {failure}", file=sys.stderr)
    if failures:
        return 1
    print(f"rainbow palette: {len(palette)} colours pass every floor")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
