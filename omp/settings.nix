# The settings oh-my-pi runs with on every profile.
#
# omp rewrites its global config.yml from `/settings` and `omp config set`,
# so Home Manager does not own that file. These keys travel in a
# `PI_CONFIG_FILES` overlay that the wrapper in package.nix adds to every
# invocation: omp loads overlays after the global file and never writes them
# back, so a declared key wins and every other key stays the program's.
{
  # Auto-approve reads and workspace writes; prompt before anything that
  # executes (bash, eval, browser, task). That is the gate OpenCode's
  # `bash = "ask"` sets; OpenCode's `external_directory = "ask"` has no
  # counterpart among omp's approval settings.
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
}
