# Prove sdcv resolves the built dictionaries offline.
#
# Copies the entries that carry a plain (uncompressed) .idx into a writable
# data dir so sdcv never has to write an index cache back into the read-only
# store, then asserts a known Mandarin, Cantonese, and Korean lookup each
# returns its expected gloss from the expected dictionary.
{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  dictionaries = import ./package.nix {inherit pkgs;};
in
  mkCheck {
    name = "dictionaries-sdcv-lookup";
    tools = [pkgs.sdcv];
    script = ''
      set -euo pipefail
      # sdcv needs a writable HOME for its cache dir; the build sandbox has no
      # UTF-8 locale, so pass the search words through as raw UTF-8 bytes.
      export HOME="$PWD"
      data="$PWD/data/dic"
      mkdir -p "$data"
      cp -r ${dictionaries}/dic/cc-cedict "$data/"
      cp -r ${dictionaries}/dic/cc-canto "$data/"
      cp -r ${dictionaries}/dic/kengdic "$data/"
      chmod -R u+w "$data"
      export STARDICT_DATA_DIR="$PWD/data"

      sdcv_lookup() {
        sdcv --non-interactive --utf8-input --utf8-output "$1"
      }

      cedict="$(sdcv_lookup 你好)"
      printf '%s\n' "$cedict"
      grep -q 'cc-cedict' <<<"$cedict"
      grep -q 'hello' <<<"$cedict"

      canto="$(sdcv_lookup 唔該)"
      printf '%s\n' "$canto"
      grep -q 'cc-canto' <<<"$canto"
      grep -q 'Jyutping: m4 goi1' <<<"$canto"

      korean="$(sdcv_lookup 사랑)"
      printf '%s\n' "$korean"
      grep -q 'kengdic' <<<"$korean"
      grep -qi 'love' <<<"$korean"
    '';
  }
