{config, ...}: let
  repo = "${config.home.homeDirectory}/dotfiles";
  liveLink = path: config.lib.file.mkOutOfStoreSymlink "${repo}/${path}";
in {
  home.file = {
    ".claude/agents".source = liveLink "agents/prompts";
    ".codex/agents".source = liveLink "agents/generated/codex-agents";
    ".codex/kernel.config.toml".source = ./generated/codex-profiles/kernel.config.toml;
    ".codex/language.config.toml".source = ./generated/codex-profiles/language.config.toml;
  };
}
