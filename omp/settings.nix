# The settings oh-my-pi runs with on every profile.
#
# omp rewrites its global config.yml from `/settings` and `omp config set`,
# so Home Manager does not own that file. These keys travel in a
# `PI_CONFIG_FILES` overlay that the wrapper in package.nix adds to every
# invocation. Declared keys override global and project settings without
# rewriting them; later user overlays and runtime overrides can still win.
{
  # Approve read/write-tier tools and prompt for exec-tier tools unless a
  # more specific policy applies. This is not a workspace boundary like
  # OpenCode's external_directory policy.
  tools.approvalMode = "write";
  # The Nix store is the update path.
  startup.checkUpdate = false;
  marketplace.autoUpdate = "off";
  # Tool-issue reports would otherwise go to the project's QA endpoint.
  dev.autoqa = false;
  # OpenCode discovers the same skills there. omp loads foreign user roots
  # only when asked, and this asks for the skills alone rather than for
  # Claude Code's whole configuration.
  skills.customDirectories = ["~/.claude/skills"];
  # The mix variant of the Gruvbox Material theme the module installs, as
  # OpenCode selects it.
  theme.dark = "gruvbox-material-mix-dark-medium";
}
