# Offline StarDict dictionaries for the sdcv console client.
#
# Every dictionary is pinned by fixed hash and reproducible.  Three shapes feed
# the same StarDict output tree at $out/dic/<name>:
#
#   * EDICT2 text (CC-CEDICT, CC-Canto) and a Tabfile (kengdic Korean) and the
#     Princeton WordNet database are converted with pyglossary at build time.
#   * FreeDict ships prebuilt StarDict archives, unpacked verbatim.
#   * The Open Vietnamese Dictionary Project ships loose StarDict files.
{pkgs}: let
  inherit (pkgs) fetchurl;

  # CC-CEDICT (Mandarin), CC BY-SA 3.0.  rhcarvalho/cedict republishes the
  # MDBG "latest" export daily; pin one commit for reproducibility.
  cedict = fetchurl {
    url = "https://raw.githubusercontent.com/rhcarvalho/cedict/fca07f88e171e8b8e54eea307d8f7a4c710c4a33/cedict_1_0_ts_utf-8_mdbg.txt";
    hash = "sha256-m8rw3CfPqLOywD50qaCM7O9uSeS2GKuP21acVk8e/E0=";
  };

  # CC-Canto (Cantonese, with Jyutping), CC BY-SA 3.0.
  canto = fetchurl {
    url = "https://raw.githubusercontent.com/amadeusine/cc-canto-data/a687e469f6d5ee6873283ad3ec6fc1b35f518465/cccanto-webdist.txt";
    hash = "sha256-6qOM4+IFVTYckDl/WHqxfaDvdclVTanqfOERANLZqSA=";
  };

  # kengdic Korean-English word list, MPL-2.0 / LGPL-2.0+.
  kengdic = fetchurl {
    url = "https://raw.githubusercontent.com/garfieldnate/kengdic/793de2369c9a98b944154eb4695d26854d2de59b/kengdic.tsv";
    hash = "sha256-0jI21WdvUGUUch5gVTRke6RzWoW7t2ueEtHsBnuM/bQ=";
  };

  # Open Vietnamese Dictionary Project, Vietnamese-English StarDict (loose
  # files, pinned at one commit of the dynamotn/stardict-vi mirror).
  viRev = "0f0b46997db2305ccd0cb9e161f25ac73988b0a9";
  viFile = name: hash:
    fetchurl {
      url = "https://raw.githubusercontent.com/dynamotn/stardict-vi/${viRev}/vi-en/${name}";
      inherit hash;
    };
  viIfo = viFile "star_vietanh.ifo" "sha256-+D9S80J3VPBnmDSUhJYGVCtpfg9cMMJHrfRYUUqYY28=";
  viIdx = viFile "star_vietanh.idx" "sha256-LN/RZUIXV6Lh317+SYBOof6yaeb8Na7pHl+NGg8bWZU=";
  viDict = viFile "star_vietanh.dict.dz" "sha256-k350tYEReP4ht0qix5vTkAWKLYykPi+JMfLUVjVcVIs=";

  # FreeDict bilingual dictionaries, prebuilt StarDict archives.  The path
  # segment percent-encodes "+"; the file component keeps the literal version.
  freedict = {
    pair,
    version,
    pathVersion ? version,
    hash,
  }:
    fetchurl {
      url = "https://download.freedict.org/dictionaries/${pair}/${pathVersion}/freedict-${pair}-${version}.stardict.tar.xz";
      inherit hash;
    };

  freedictDicts = {
    "deu-eng" = freedict {
      pair = "deu-eng";
      version = "1.9-fd1";
      hash = "sha256-eaOGevDqBT8ryUJc9k1Ev+BuM6snQbzKNWfVaWP51aE=";
    };
    "fra-eng" = freedict {
      pair = "fra-eng";
      version = "0.4.1";
      hash = "sha256-SPFC1yeAkNbtDUIAH3Y0xM4/NlHy0XFGhGaDA/gnE4U=";
    };
    "ita-eng" = freedict {
      pair = "ita-eng";
      version = "2025.11.23";
      hash = "sha256-48Nky10yw/O5vqvmYiwyDKv/yLrEc0SlvwZHVYnoI9M=";
    };
    "pol-eng" = freedict {
      pair = "pol-eng";
      version = "2025.11.23";
      hash = "sha256-tjNXnDB2arAPwQoOYn8TloPkPBB/EIt8YUN1VjfxHg4=";
    };
    "spa-eng" = freedict {
      pair = "spa-eng";
      version = "0.3.1";
      hash = "sha256-CUoN8jXJtiftGvDvn7WiZZYr3pY7Z8IdYjomnDm5yy0=";
    };
    "epo-eng" = freedict {
      pair = "epo-eng";
      version = "2024.10.06+fd1";
      pathVersion = "2024.10.06%2Bfd1";
      hash = "sha256-qDCq4LNmm63BQz3+PZkhH2DPcnbfVxwm6TRCqYGs4sU=";
    };
    "jpn-eng" = freedict {
      pair = "jpn-eng";
      version = "0.1";
      hash = "sha256-1k3xtSa6sX/dRg5levAYfjNXqMBHYET7S/ycxwAM5po=";
    };
  };

  installFreedict =
    pkgs.lib.concatStringsSep "\n"
    (pkgs.lib.mapAttrsToList (name: src: "install_freedict ${src} ${name}") freedictDicts);
in
  pkgs.stdenvNoCC.mkDerivation {
    pname = "stardict-dictionaries";
    version = "2026-07-25";

    dontUnpack = true;

    nativeBuildInputs = [
      pkgs.pyglossary
      pkgs.python3
      pkgs.gnutar
      pkgs.xz
      pkgs.gzip
    ];

    buildCommand = ''
      set -euo pipefail
      export HOME="$TMPDIR"
      dic="$out/dic"
      mkdir -p "$dic"

      convert() {
        # convert READ_FORMAT NAME INPUT
        mkdir -p "$dic/$2"
        pyglossary --ui=none --no-progress-bar \
          --read-format="$1" --write-format=Stardict --name="$2" \
          "$3" "$dic/$2/$2.ifo"
      }

      install_freedict() {
        # install_freedict TARBALL NAME
        local tmp
        tmp="$(mktemp -d)"
        tar -xf "$1" -C "$tmp"
        mkdir -p "$dic/$2"
        find "$tmp" -type f \( \
            -name '*.ifo' -o -name '*.idx' -o -name '*.idx.gz' \
            -o -name '*.dict' -o -name '*.dict.dz' \
            -o -name '*.syn' -o -name '*.syn.dz' \) \
          -exec cp {} "$dic/$2/" \;
        rm -rf "$tmp"
      }

      # EDICT2 text -> StarDict (Mandarin, Cantonese).
      convert EDICT2 cc-cedict ${cedict}
      python3 ${./canto_prep.py} ${canto} cccanto.u8
      convert EDICT2 cc-canto cccanto.u8

      # Tabfile -> StarDict (Korean, from kengdic TSV).
      python3 ${./kengdic_prep.py} ${kengdic} kengdic.tab
      convert Tabfile kengdic kengdic.tab

      # WordNet database -> StarDict (English monolingual).
      convert Wordnet wordnet ${pkgs.wordnet}/dict

      # Open Vietnamese Dictionary Project, loose StarDict files.
      mkdir -p "$dic/vi-en"
      cp ${viIfo} "$dic/vi-en/star_vietanh.ifo"
      cp ${viIdx} "$dic/vi-en/star_vietanh.idx"
      cp ${viDict} "$dic/vi-en/star_vietanh.dict.dz"

      # FreeDict prebuilt StarDict archives.
      ${installFreedict}
    '';

    meta = {
      description = "Offline StarDict dictionaries for sdcv (CC-CEDICT, CC-Canto, FreeDict, OVDP, kengdic, WordNet)";
      platforms = pkgs.lib.platforms.linux;
    };
  }
