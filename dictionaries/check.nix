# Prove sdcv reads every built dictionary straight from the read-only store,
# as STARDICT_DATA_DIR deploys it, with one known lookup per dictionary.
{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  dictionaries = import ./package.nix {inherit pkgs;};
in
  # The reader and the data live in different modules: home.nix installs sdcv,
  # this module points it at the built tree through STARDICT_DATA_DIR. Assert
  # every profile keeps both, so dropping either is caught here rather than
  # leaving the lookup below testing a reader the profile no longer ships.
  assert lib.all (home: lib.elem pkgs.sdcv home.config.home.packages) (lib.attrValues homes);
  assert lib.all (
    home: home.config.home.sessionVariables.STARDICT_DATA_DIR == "${dictionaries}"
  ) (lib.attrValues homes);
    mkCheck {
      name = "dictionaries-sdcv-lookup";
      tools = [pkgs.sdcv];
      script = ''
        set -euo pipefail
        # sdcv needs a HOME for its cache dir; the build sandbox has no UTF-8
        # locale, so pass the search words through as raw UTF-8 bytes.
        export HOME="$PWD"
        export STARDICT_DATA_DIR=${dictionaries}

        # dictionary|word|expected gloss; the dictionary is its .ifo bookname.
        lookups=(
          'cc-cedict|你好|hello'
          'cc-canto|唔該|Jyutping: m4 goi1'
          'kengdic|사랑|love'
          'wordnet|love|sexual'
          'German - English Ding/FreeDict dictionary (de-en)|Haus|house'
          'French-English FreeDict Dictionary (fr-en)|maison|house'
          'italiano-English FreeDict+WikDict dictionary (it-en)|casa|house'
          'Spanish-English FreeDict Dictionary (es-en)|casa|house'
          'język polski-English FreeDict+WikDict dictionary (pl-en)|dom|house'
          'Esperanto-English FreeDict dictionary (eo-en)|domo|house'
          'Japanese-English FreeDict Dictionary (ja-en)|猫|cat'
          'Từ điển Việt-Anh|chào|greet'
        )
        test "$(find ${dictionaries}/dic -name '*.ifo' | wc -l)" = ''${#lookups[@]}
        for lookup in "''${lookups[@]}"; do
          IFS='|' read -r dictionary word gloss <<<"$lookup"
          # --use-dict converts the name through the locale, which fails here
          # for non-ASCII names, so take this dictionary's block of the output.
          result=$(sdcv --non-interactive --exact-search --utf8-input \
            --utf8-output "$word" 2>/dev/null |
            awk -v header="-->$dictionary" '
              /^-->/ && previous !~ /^-->/ { keep = $0 == header }
              keep { print }
              { previous = $0 }
            ')
          if ! grep -qiF -- "$gloss" <<<"$result"; then
            printf '%s: no "%s" for %s\n' "$dictionary" "$gloss" "$word" >&2
            exit 1
          fi
        done
      '';
    }
