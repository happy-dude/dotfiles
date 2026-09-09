# Dispatch a hook name to the repository's own hook.
#
# With core.hooksPath set, Git looks for every hook only in that directory,
# so a repository's .git/hooks are silently skipped unless each name here
# hands over to them. The dispatcher is installed under every documented
# hook name except the protocol and override hooks; commit-msg has its own
# dispatcher that also runs the linter.
{pkgs}:
pkgs.writeShellApplication {
  name = "git-local-hook";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.git
  ];
  text = ''
    hook_name=''${0##*/}
    # reference-transaction runs while git init is still creating the
    # repository, where nothing resolves yet and a failing hook aborts the
    # command; with no repository there is no hook to hand over to.
    common_dir=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null) ||
      exit 0
    local_hook="$common_dir/hooks/$hook_name"
    if [[ -x $local_hook && \
      $(readlink -f "$local_hook") != $(readlink -f "$0") ]]; then
      exec "$local_hook" "$@"
    fi
  '';
}
