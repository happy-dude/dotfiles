{
  lib,
  pkgs,
  ...
}: let
  prompts = import ../agents/prompts.nix {inherit lib;};
  settings = import ./settings.nix;
  omp = import ./package.nix {inherit pkgs settings;};
  # omp discovers task agents from ~/.omp/agent/agents/*.md, taking `name`
  # and `description` from YAML frontmatter and the rest of the file as the
  # system prompt. It skips ~/.claude/agents deliberately, because Claude's
  # frontmatter is a different contract, so the canonical prompts are
  # re-emitted here with only the fields omp defines. The description is
  # written as a JSON string, which YAML reads as a quoted scalar.
  agentFiles =
    lib.mapAttrs (
      name: prompt:
        pkgs.writeText "omp-agent-${name}.md" ''
          ---
          name: ${name}
          description: ${builtins.toJSON prompt.description}
          ---

          ${prompt.body}
        ''
    )
    prompts;
in {
  home.packages = [omp];
  home.file =
    lib.mapAttrs' (
      name: source:
        lib.nameValuePair ".omp/agent/agents/${name}.md" {inherit source;}
    )
    agentFiles;
}
