{lib}: let
  agentNames = [
    "kernel"
    "language"
  ];

  parsePrompt = name: let
    lines = lib.splitString "\n" (
      builtins.readFile (./prompts + "/${name}.md")
    );
    indexed = lib.imap0 (index: value: {inherit index value;}) lines;
    closing =
      lib.findFirst (
        line: line.index > 0 && line.value == "---"
      )
      null
      indexed;
    frontmatter = lib.take (closing.index + 1) lines;
    metadata =
      lib.foldl' (
        state: line:
          if lib.hasPrefix "name: " line
          then
            state
            // {
              name = lib.removePrefix "name: " line;
              readingDescription = false;
            }
          else if line == "description:"
          then state // {readingDescription = true;}
          else if state.readingDescription && lib.hasPrefix "  " line
          then
            state
            // {
              descriptionLines =
                state.descriptionLines ++ [(lib.strings.trim line)];
            }
          else state // {readingDescription = false;}
      ) {
        name = null;
        descriptionLines = [];
        readingDescription = false;
      }
      frontmatter;
    body = lib.strings.trim (lib.concatStringsSep "\n" (
      lib.drop (closing.index + 1) lines
    ));
  in
    assert lines != [] && builtins.head lines == "---";
    assert closing != null;
    assert metadata.name == name;
    assert metadata.descriptionLines != [];
    assert body != ""; {
      inherit name body;
      description = lib.concatStringsSep " " metadata.descriptionLines;
    };
in
  lib.genAttrs agentNames parsePrompt
