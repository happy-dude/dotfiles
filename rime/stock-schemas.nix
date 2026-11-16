# The lists in the locked stock schemas that the tracked patches depend on.
#
# cangjie5_colemak.schema.yaml inserts its Lua components by list position,
# and jyut6ping3.custom.yaml replaces the filter list outright. Rime applies
# both to whatever the stock schema holds, so after a lock update that moved
# or added an entry, a component would land in the wrong place or the new
# entry would be dropped, and nothing would report it. rime/default.nix fails
# evaluation on `problems` instead.
{lib}: let
  # What each patch was written against, by deployed file and list.
  writtenAgainst = {
    "cangjie5.schema.yaml" = {
      "engine/processors" = [
        "ascii_composer"
        "recognizer"
        "key_binder"
        "speller"
        "punctuator"
        "selector"
        "navigator"
        "express_editor"
      ];
      "engine/filters" = [
        "simplifier"
        "uniquifier"
        "single_char_filter"
      ];
    };
    "jyut6ping3.schema.yaml" = {
      "engine/filters" = [
        "simplifier@emoji_cantonese_suggestion"
        "simplifier"
        "simplifier@variants_hk"
        "simplifier@trad_tw"
        "uniquifier"
        "reverse_lookup_filter@jyut6ping3_reverse_lookup"
      ];
    };
  };

  topLevelKey = "([A-Za-z_][A-Za-z0-9_]*):.*";
  nestedKey = "  ([A-Za-z_][A-Za-z0-9_]*):[[:space:]]*(#.*)?";
  # The value ends at whitespace followed by `#`, or at the end of the line.
  listItem = "    - ([^[:space:]#]([^#]*[^[:space:]#])?)[[:space:]]*(#.*)?";

  # The block lists written as
  #
  #   section:
  #     key:
  #       - item  # comment
  #
  # keyed by "section/key". That is how the stock schemas write the lists
  # named above; a list written any other way is not found, which `problems`
  # reports like any other difference.
  blockLists = text:
    (lib.foldl' (
        state: line: let
          top = builtins.match topLevelKey line;
          nested = builtins.match nestedKey line;
          item = builtins.match listItem line;
        in
          if top != null
          then
            state
            // {
              section = builtins.head top;
              list = null;
            }
          else if nested != null && state.section != null
          then let
            list = "${state.section}/${builtins.head nested}";
          in
            state
            // {
              inherit list;
              lists = state.lists // {${list} = [];};
            }
          else if item != null && state.list != null
          then state // {lists = state.lists // {${state.list} = state.lists.${state.list} ++ [(builtins.head item)];};}
          else if builtins.match "[[:space:]]*(#.*)?" line != null
          then state
          else state // {list = null;}
      ) {
        section = null;
        list = null;
        lists = {};
      } (lib.splitString "\n" text)).lists;

  show = list:
    if list == null
    then "missing or not a block list"
    else "[${lib.concatStringsSep ", " list}]";

  # entries: the deployed files as { relative, path }.
  # expected: the lists to hold them to; the tests pass their own.
  # Returns one line per difference; an empty list means none.
  problems = {
    entries,
    expected ? writtenAgainst,
  }:
    lib.concatLists (
      lib.mapAttrsToList (
        file: lists: let
          sources = lib.filter (entry: entry.relative == file) entries;
          found = blockLists (builtins.readFile (builtins.head sources).path);
        in
          if sources == []
          then ["${file} is not deployed"]
          else
            lib.concatLists (
              lib.mapAttrsToList (
                name: list:
                  lib.optional ((found.${name} or null) != list)
                  "${file}: ${name} is ${show (found.${name} or null)}; expected ${show list}"
              )
              lists
            )
      )
      expected
    );
in {
  inherit problems writtenAgainst;
}
