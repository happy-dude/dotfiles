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
    file = "agents/prompts/${name}.md";
    require = condition: message: lib.throwIfNot condition "${file}: ${message}";
  in
    require (lines != [] && builtins.head lines == "---")
    "the first line must open the frontmatter with ---"
    require (closing != null)
    "the frontmatter has no closing --- line"
    require (metadata.name == name)
    "frontmatter name must be ${name}"
    require (metadata.descriptionLines != [])
    "frontmatter description must be a block of indented lines after description:"
    require (body != "")
    "the prompt body after the frontmatter is empty"
    {
      inherit name body;
      description = lib.concatStringsSep " " metadata.descriptionLines;
    };
in
  lib.genAttrs agentNames parsePrompt
