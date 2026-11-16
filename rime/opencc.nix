# Which OpenCC configs the deployed Rime YAML names, and whether each exists.
#
# A simplifier filter whose `opencc_config` cannot be found converts nothing.
# Rime logs the missing file and carries on, so the filter stays in the schema
# and silently does nothing. rime/default.nix fails evaluation on `problems`
# instead.
#
# Every deployed schema and patch is read, enabled or not. The enabled list is
# assembled by Rime's own patch rules, which evaluation does not replay, and a
# schema that is deployed but switched off would otherwise break on the day it
# is turned on.
{lib}: let
  # The configs pkgs.opencc installs. OpenCC resolves a bare name against its
  # own data directory when neither of Rime's opencc/ directories has the
  # file. The rime-opencc-configs check holds this list against the package,
  # so evaluation never reads it.
  stockConfigs = [
    "hk2s.json"
    "hk2sp.json"
    "hk2t.json"
    "jp2t.json"
    "s2hk.json"
    "s2hkp.json"
    "s2t.json"
    "s2tw.json"
    "s2twp.json"
    "t2hk.json"
    "t2jp.json"
    "t2s.json"
    "t2tw.json"
    "tw2s.json"
    "tw2sp.json"
    "tw2t.json"
  ];

  # Configs a deployed schema names that are absent on purpose.
  absentOnPurpose = [
    # Upstream's Cantonese schema lists `simplifier@emoji_cantonese_suggestion`
    # and ships its switch off. The emoji data is a separate upstream add-on
    # that this repository has never installed, so the filter is inert on
    # purpose.
    "emoji_cantonese.json"
  ];

  # Matches a nested `opencc_config: name` key and a patch's quoted or bare
  # `section/opencc_config: name` path. A `#` before the key means the line is
  # a comment.
  openccConfigLine = ''[^#]*opencc_config["']?[[:space:]]*:[[:space:]]*["']?([^"'#[:space:]]+).*'';

  # Dictionaries are large, name no configs, and may come from a package whose
  # output evaluation must not read.
  namesConfigs = relative: lib.hasSuffix ".yaml" relative && !(lib.hasSuffix ".dict.yaml" relative);

  configsNamedIn = path:
    lib.concatMap (
      line: let
        found = builtins.match openccConfigLine line;
      in
        lib.optional (found != null) (builtins.head found)
    ) (lib.splitString "\n" (builtins.readFile path));

  # entries: the deployed files as { relative, path }.
  # absent: the configs allowed to be missing; the tests pass their own.
  # Returns one line per problem; an empty list means every name resolves.
  problems = {
    entries,
    absent ? absentOnPurpose,
  }: let
    provided = name: builtins.any (entry: entry.relative == "opencc/${name}") entries;
    named = lib.concatMap (
      entry:
        map (config: {
          inherit config;
          inherit (entry) relative;
        }) (configsNamedIn entry.path)
    ) (lib.filter (entry: namesConfigs entry.relative) entries);
    isNamed = name: builtins.any (reference: reference.config == name) named;
  in
    map (reference: "${reference.relative} names ${reference.config}, which no source provides") (
      lib.filter (
        reference:
          !(provided reference.config
            || builtins.elem reference.config stockConfigs
            || builtins.elem reference.config absent)
      )
      named
    )
    ++ map (name: "absentOnPurpose lists ${name}, which a source now provides") (
      lib.filter provided absent
    )
    ++ map (name: "absentOnPurpose lists ${name}, which no deployed file names any more") (
      lib.filter (name: !(isNamed name)) absent
    );
in {
  inherit absentOnPurpose problems stockConfigs;
}
